{}{
 FlushFileBuffers перед tryread (хэндл у нас остался), попробовать с named pipe

    PeekNamedPipe(FHandles[ehStdOut], nil, 0, nil, @ToRead, nil);
    // если ничего не прочитано и процесс завершился, закрываем трубу
    if ToRead = 0 then
    begin
      if FProcessId = 0 then CloseAndZeroHandle(FHandles[ehStdOut]);
    end
    else
//

  piped command : Launch( [ 'cmd /c dir', 'gzip -9', 'gzip --decompress' ] )
  with checking status & exit codes

 }

{*******************************************************************************
              Эмулятор консоли для запуска консольных приложений
                      с перенаправлением ввода/вывода.

                     © Fr0sT, fr0st.brutal@gmail.com

  Возможности:
    * Как визуальный, так и невизуальный (без Forms) вариант. Для
        использования визуального варианта надо объявить дефайн ConEm_VCL
    * Запуск любых консольных программ, batch файлов и т.д.
    * Получение вывода
    * Передача переменных окружения
    * Ввод команд после запуска программы
    * Запись данных в STDIN запущенной программы
    * Отслеживание таймаута неактивности
    * Стандартизованный вывод сообщений об ошибках (должна быть поддержка в
        запускаемых программах/скриптах)
    * Запись выводимых данных в лог файл
    * Неблокирующие операции чтения-записи

*******************************************************************************}

unit ConsoleEmulator;

interface

uses Classes, Windows, SysUtils, Messages, StrUtils,
     {$IFDEF ConEm_VCL}
     ExtCtrls, Forms, Graphics, Controls, StdCtrls,
     {$ENDIF}
     Utils;

{$REGION 'Notes'}
{========= IMPORTANT NOTES ON PIPES =========

   SCHEME OF CONSOLE EMULATOR
     Usual console app:
        STDIN =====> [APP] =====> STDOUT
                           =====> STDERR

     Console app launched with console emulator:
        CONS_EM.InputStream =====> [APP] =====> CONS_EM.OuputStream
        CONS_EM.DataInput()                     CONS_EM.OnDataOutput()

   DELAYED OUTPUT / PIPE BUFFERING
     If you encounter delays in launched command's output, that's the case.
     Windows buffers pipe output to internal memory and writes data when the
     buffer fills. Compilator-specific IO libs may have their own buffering as
     well. Alas, there's nothing we can do with it.

   WAIT FOR INPUT / PIPE EOF
     Console apps that read input from STDIN continue reading until they encounter
     EOF (IOW, ReadFile returns -1). This happens only when the input channel
     closes. You can control this manually with CloseInput method or automatically
     by setting AutoCloseInput parameter in Launch() method to True. In this case
     STDIN will be closed when InputStream reaches its end. There's no means to
     reopen input channel after closing, that's why InputStream setting is available
     only in Launch().

=============================================}
{$ENDREGION}

type
  THandles = (ehStdIn,     // STDIN pipe handle
              ehStdOut,    // STDOUT pipe handle
              ehProcess);  // Process handle for checking state

  TConsEmulState = (cesWaiting, cesRunning, cesFinished, cesTerminated, cesTerminatedByTimeout);

  TIODir = (ioInput, ioOutput);

  // Class which implements command line execution
  TConsoleEmulator = class
  strict private
    FHWnd: HWND; // owns timer for reading data and tracking timeout
    FInputBuf, FOutputBuf: TBytes;
    FProcExitCode: Cardinal;
    FTimeout: Cardinal;  // [sec]
    FHandles: array[THandles] of THandle;
    FProcessId: THandle;
    FState: TConsEmulState;
    FLaunchTick, FLastActiveTick: Cardinal;
    FCmdLine: string;
    FInputStm, FOutputStm: TStream;
    FAutoCloseInput: Boolean;
    const
      BufSize = 16*1024;
      EventIDs: array[TIODir] of UINT_PTR = (100, 101);
      TimerIntervals: array[TIODir] of UINT = (500, 300);

    function WndProc(wnd: HWND; msg: UINT; wPar: WPARAM; lPar: LPARAM): LRESULT;
    procedure SetState(NewState: TConsEmulState);
    procedure ClearHandles;
    procedure SetTimer(IODir: TIODir; Enabled: Boolean);
    procedure TryWrite;
    procedure TryRead;
  public
    OnDataOutput : procedure(Sender: TConsoleEmulator; Data: PByte; DataLen: Integer) of object;
    OnStateChange : procedure(Sender: TConsoleEmulator; State: TConsEmulState; ExitCode: Cardinal) of object;
    OnIOError : procedure(Sender: TConsoleEmulator; IODirection: TIODir; ErrCode: Cardinal) of object;

    constructor Create(ProcTimeout: Cardinal); reintroduce;
    destructor Destroy; override;
    procedure Launch(CmdLine: string; CurrDir: string = ''; EnvVars: string = '';
                     InputStm: TStream = nil; AutoCloseInput: Boolean = False);
    procedure Terminate(ByTimeout: Boolean = False);

    function DataInput(const Data; DataLen: Integer): Integer;
    procedure SendCmd(const Cmd: string);
    procedure CloseInput;

    property State: TConsEmulState read FState;
    property ExitCode: Cardinal read FProcExitCode;
    property CmdLine: string read FCmdLine;
    property OutputStm: TStream read FOutputStm write FOutputStm;
    property InputStm: TStream read FInputStm;
  end;

  {$IFDEF ConEm_VCL}
  // Console form with logging of output, command input, status labels and so on
  TfrmConsole = class(TCustomForm)
  private
    // components
    mConsole: TMemo;
    eCommand: TEdit;
    lblState, lblEmulCallback: TLabel;
    // fields
    FMemoLineCompl, FThisLineCompl: Boolean;
    FConsEmul: TConsoleEmulator;
    FLogFile: IFileWriter;
    FExitCode: Cardinal;
    FErrorMsg: string;
    // property g/setters
    function GetLogFileName: string;
    procedure SetLogMaxFileSize(Val: Int64);
    // events
    procedure btnSendCmdClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnAbortClick(Sender: TObject);
    // event handers
    procedure ConsEmulOnDataOutput(Sender: TConsoleEmulator; Data: PByte; DataLen: Integer);
    procedure ConsEmulOnStateChange(Sender: TConsoleEmulator; State: TConsEmulState; ExitCode: Cardinal);
  public
    OnProcessFinished: procedure(Sender: TfrmConsole; State: TConsEmulState) of object;
    property ExitCode: Cardinal read FExitCode;
    property ErrorMsg: string read FErrorMsg;
    property LogFileName: string read GetLogFileName;
    property LogMaxFileSize: Int64 write SetLogMaxFileSize;
    constructor Create(AOwner: TComponent; ATimeout: Cardinal); reintroduce;
    destructor Destroy; override;
    procedure Log(Msg: string);
    function Launch(const CmdLine: string; LogFN: string = ''; CurrDir: string = ''; EnvVars: string = ''): Boolean;
    function IsRunning: Boolean;
  end;
  {$ENDIF}

const
  ErrSignName = 'ConEm_Err';  // имя переменной окружения, содержащей сигнатуру ошибки
  ErrSign = '*ERROR*';        // сигнатура ошибки. Если запускаемая программа выводит сообщение
                              //   с этой сигнатурой, это сообщение будет распознано эмулятором
                              //   и занесено в поле ErrorMsg

function Execute(CmdLine: string; CurrDir: string; EnvVars: string;
                 InputStm: TStream; OutputStm: TStream; Timeout: Cardinal): DWORD;

implementation

uses TlHelp32;

const // localizable
  S_AlreadyLaunched = 'Launch: процесс уже запущен!';
  S_NotLaunched = 'SendCmd: Процесс не запущен!';
  {$IFDEF ConEm_VCL}
  S_FormCaption = 'Эмулятор консоли';
  S_BtnSendCaption = 'Отправить';
  S_BtnAbortCaption = 'Прервать';
  S_ConsEmulStateLabels: array[TConsEmulState] of string =
    ('ожидает', 'запущен', 'завершён', 'остановлен', 'остановлен по таймауту');
  S_LblStatePatt = 'Состояние: %s        Командная строка: %s';
  S_ProcessStatePatt = '<=== %s Процесс %s%s ===>';
  S_ExitCodePatt = ' (Код %d%s)';
  S_ErrMsgPatt = ', сообщение "%s"';
  S_ErrLaunching = '! Ошибка запуска "%s": %s';
  {$ENDIF}

// Функция завершает процесс вместе со всеми дочерними
function KillProcessTree(ProcId: DWORD): Integer;
var Snapshot: Cardinal;
    PrEntry: PROCESSENTRY32;
    hProc: Cardinal;
begin
  // получаем слепок
  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snapshot = INVALID_HANDLE_VALUE then
    begin Result := GetLastError; Exit; end;
  // получаем первый процесс
  PrEntry.dwSize := SizeOf(PrEntry);
  if not Process32First(Snapshot, PrEntry) then
    begin Result := GetLastError; CloseHandle(Snapshot); Exit; end;
  // убиваем все процессы, порождённые текущим
  repeat
    if PrEntry.th32ParentProcessID = ProcId then
      KillProcessTree(PrEntry.th32ProcessID);
  until not Process32Next(Snapshot, PrEntry);
  CloseHandle(Snapshot);
  // и завершаем исходный процесс
  hProc := OpenProcess(PROCESS_TERMINATE, False, ProcId);
  if hProc <> 0 then TerminateProcess(hProc, High(DWORD)-1);
  Result := GetLastError;
  CloseHandle(hProc);
end;

// Execute console application or script
//   CmdLine              - command to execute
//   CurrDir   [opt]      - current dir for command
//   EnvVars   [opt]      - custom environment variables, "v1=val1;v2=val2;..."
//   InputStm  [opt]      - stream to write to STDIN
//   OutputStm [opt]      - stream to read from STDOUT

// ? очень странно. если неблок на вывод - ООООчень долго. чтение по мелким кусочкам.
// но если блок и без внутреннего цикла - также чтение по мелким кусочкам


function Execute(CmdLine: string; CurrDir: string; EnvVars: string;
                 InputStm: TStream; OutputStm: TStream; Timeout: Cardinal): DWORD;
const
  BufSize = 16*1024;
  MaxLoopTime = 100; // [ms] максимальное время
var  // общие для всех процедур переменные
  ProcessId: THandle;
  InputBuf, OutputBuf: TBytes;
  Handles: array[THandles] of THandle;
  LastActiveTick: Cardinal;

procedure Launch(CmdLine: string; CurrDir: string; EnvVars: string);
var
  si: TStartupInfo;
  pi: TProcessInformation;
  sa: TSecurityAttributes;
  pOldEnv, tmp: PChar;
  OldEnvLen, NewEnvLen, err: Integer;
  hStdOut, hStdIn: THandle;
  mode: DWORD;
  IntCmdLine: string;
begin
  ZeroMem(si, SizeOf(si));
  ZeroMem(pi, SizeOf(pi));

  try
    // TSecurityAttributes для процесса и труб
    ZeroMem(sa, SizeOf(sa));
    sa.nLength := SizeOf(sa);
    sa.lpSecurityDescriptor := nil;
    sa.bInheritHandle := True;
    // create pipes
    mode := PIPE_READMODE_BYTE or PIPE_NOWAIT;
    // STDOUT
    if not CreatePipe(Handles[ehStdOut], hStdOut, @sa, 1) then
      Error('CreatePipe: '+LastErrMsg);
    // Ensure the read handle to the pipe for STDOUT is not inherited (from MSDN example)
    SetHandleInformation(Handles[ehStdOut], HANDLE_FLAG_INHERIT, 0);
    // Set non-blocking R/W mode for the pipe (!)
//    SetNamedPipeHandleState(Handles[ehStdOut], mode, nil, nil);
    // STDIN
    if not CreatePipe(hStdIn, Handles[ehStdIn], @sa, 1) then
      Error('CreatePipe: '+LastErrMsg);
    // Ensure the write handle to the pipe for STDIN is not inherited (from MSDN example)
    SetHandleInformation(Handles[ehStdIn], HANDLE_FLAG_INHERIT, 0);
    // Set non-blocking R/W mode for the pipe (!)
    SetNamedPipeHandleState(Handles[ehStdIn], mode, nil, nil);

    // заполняем структуры для создания процесса
    si.cb          := SizeOf(si);
    si.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    si.wShowWindow := SW_HIDE;
    si.hStdInput   := hStdIn;
    si.hStdOutput  := hStdOut;
    si.hStdError   := hStdOut;
    if CurrDir = '' then CurrDir := GetCurrentDir;

    // Конструируем новое окружение из переданной в параметре строки, сразу добавляя
    // и сигнатуру сообщения об ошибке. Соответственно EnvVars у нас всегда непуст,
    // и проверки if (EnvVars <> '') можно убрать
    EnvVars := ErrSignName + '=' + ErrSign + ';' + EnvVars;
    if EnvVars[Length(EnvVars)] <> ';' then EnvVars := EnvVars + ';';
    EnvVars := StringReplace(EnvVars, ';', #0, [rfReplaceAll]);
    NewEnvLen := Length(EnvVars);

    // Получаем старые переменные окружения, определяем их длину
    // (НЕ включая завершающий второй ноль, чтобы обработать случай пустой строки)
    pOldEnv := GetEnvironmentStrings;
    if pOldEnv <> nil then
    begin
      tmp := pOldEnv;
      while not ( (tmp^ = #0) and (CharNext(tmp)^ = #0) ) do Inc(tmp, StrLen(tmp)+1);
      OldEnvLen := tmp - pOldEnv;
    end
    else
      OldEnvLen := 0;

    // Собираем новый список переменных окружения, в конец вручную добавляем второй ноль
    SetLength(EnvVars, NewEnvLen + OldEnvLen + 1);
    Move(pOldEnv^, EnvVars[NewEnvLen + 1], OldEnvLen*SizeOf(Char));
    EnvVars[Length(EnvVars)] := #0;
    FreeEnvironmentStrings(pOldEnv);

    IntCmdLine := CmdLine; // обеспечиваем изменяемость комстроки - особенности CreateProcessW
    if not CreateProcess(nil, PChar(IntCmdLine), @sa, nil, True,
                         CREATE_NEW_CONSOLE{$IFDEF UNICODE} or CREATE_UNICODE_ENVIRONMENT{$ENDIF},
                         PChar(EnvVars), PChar(CurrDir), si, pi) then
    begin
      err := GetLastError;
      Error('CreateProcess: '+SysErrorMessage(err)+' ['+IntToStr(err)+'], "'+IntCmdLine+'"');
    end;
    ProcessId := pi.dwProcessId;
    Handles[ehProcess] := pi.hProcess;
  finally
    // освобождаем хэндлы потока и уже унаследованных концов труб
    CloseAndZeroHandle(hStdIn);
    CloseAndZeroHandle(hStdOut);
    CloseAndZeroHandle(pi.hThread);
  end;
end;

procedure TryWrite(InputStm: TStream);
var
  InputPtr: PByte;
  ToWrite: Integer;
  bytes, StartTick: Cardinal;
  res: Boolean;
begin
  if Handles[ehStdIn] <> 0 then
  begin
    // copy input data from stream to buffer
    ToWrite := InputStm.Read(InputBuf[0], Length(InputBuf));
    if ToWrite <= 0 then // nothing to write
    begin
      CloseAndZeroHandle(Handles[ehStdIn]);
      Exit;
    end;
    InputPtr := @InputBuf[0];
    StartTick := GetTickCount;
    // write data in a loop
    repeat
      res := WriteFile(Handles[ehStdIn], InputPtr^, ToWrite, bytes, nil);
      if not res then
        case GetLastError of
          ERROR_NO_DATA,      // here: pipe closed
          ERROR_BROKEN_PIPE:  // pipe closed on the other end
            CloseAndZeroHandle(Handles[ehStdIn]);
          else                // other error - report & close
          begin
            CloseAndZeroHandle(Handles[ehStdIn]);
            Error('TryWrite: '+LastErrMsg);
          end;
        end; // case
      if bytes = 0 then Break;

      // if something was read, regardless the error, process it
      LastActiveTick := GetTickCount;
      Inc(InputPtr, bytes);
      Dec(ToWrite, bytes);

      if not res then Break; // if WriteFile failed, break
      // control loop execution time
      if TicksSince(StartTick) > MaxLoopTime then Break;
    until False;
    // ToWrite is amount of data unwritten so rewind the stream (hoping it supports that!)
    if ToWrite > 0 then
      InputStm.Seek(-ToWrite, soCurrent);
  end; // if
end;

procedure TryRead(OutputStm: TStream);
var
  bytes, StartTick: Cardinal;
  res: Boolean;
begin
//WriteLnToFile('console.log', FormatDateTime('hh:mm:ss.zzz', now)+ ' << TryRead');
  // read data from pipe in a loop
  if Handles[ehStdOut] <> 0 then
  begin
    StartTick := GetTickCount;
    repeat
      // read data, on error close the {}handle and break the loop
      res := ReadFile(Handles[ehStdOut], OutputBuf[0], Length(OutputBuf), bytes, nil);
      if not res then
        case GetLastError of
          ERROR_NO_DATA:      // here: pipe is currently empty, that's OK
            ;
          ERROR_BROKEN_PIPE:  // pipe closed on the other end
            CloseAndZeroHandle(Handles[ehStdOut]);
          else                // other error - report & close
          begin
            CloseAndZeroHandle(Handles[ehStdOut]);
            Error('TryRead: '+LastErrMsg);
          end;
        end; // case
      if bytes = 0 then Break;
//WriteLnToFile('console.log', FormatDateTime('hh:mm:ss.zzz', now)+ ' read '+itos(bytes));

      // if something was read, regardless the error, process it
      LastActiveTick := GetTickCount;
      OutputStm.Write(OutputBuf[0], bytes);

      if not res then Break; // if ReadFile failed, break
      // control loop execution time
      if TicksSince(StartTick) > MaxLoopTime then Break;
    until False;
  end; // if
//WriteLnToFile('console.log', FormatDateTime('hh:mm:ss.zzz', now)+ ' TryRead >>');
end;

var h: THandles;
begin
  Result := 0;

  Launch(CmdLine, CurrDir, EnvVars);

  LastActiveTick := GetTickCount;
  if InputStm <> nil then
    SetLength(InputBuf, BufSize);
  SetLength(OutputBuf, BufSize);

  // основной цикл
  Sleep(200);
  repeat
    if InputStm <> nil then
      TryWrite(InputStm);
    TryRead(OutputStm);

    if (not GetExitCodeProcess(Handles[ehProcess], Result)) or (Result <> STILL_ACTIVE) then
      Break
    // если нет - проверяем, не истёк ли таймаут неактивности
    else if Timeout <> 0 then
      if TicksSince(LastActiveTick) >= Timeout*MSecsPerSec then
      begin
        KillProcessTree(ProcessId);
        Result := 1;
      end;

     Sleep(200);
  until False;

  for h := Low(THandles) to High(THandles) do
    CloseAndZeroHandle(Handles[h]);
end;

{$REGION 'TConsoleEmulator'}

constructor TConsoleEmulator.Create(ProcTimeout: Cardinal);
begin
  inherited Create;
  FHWnd := AllocateMsgWnd(WndProc);
  if FHWnd = 0 then
    Error('AllocateMsgWnd: '+LastErrMsg);
  SetTimer(ioOutput, True);
  SetLength(FOutputBuf, BufSize);

  FTimeout := ProcTimeout;
  FState := cesWaiting;
end;

destructor TConsoleEmulator.Destroy;
begin
  Terminate;
  DestroyWindow(FHWnd);
  inherited;
end;

// Меняем состояние и вызываем обработчик этого события, если он присвоен
procedure TConsoleEmulator.SetState(NewState: TConsEmulState);
begin
  if FState = NewState then Exit;
  FState := NewState;
  if Assigned(OnStateChange) then
    OnStateChange(Self, FState, FProcExitCode);
  // если процесс так или иначе завершился - отключаем таймеры, меняем состояние на ожидающее
  if FState in [cesFinished, cesTerminated, cesTerminatedByTimeout] then
  begin
    SetTimer(ioInput, False);
    SetTimer(ioOutput, False);
    SetState(cesWaiting);
  end;
end;

procedure TConsoleEmulator.SetTimer(IODir: TIODir; Enabled: Boolean);
begin
  if not Enabled then
    KillTimer(FHWnd, EventIDs[IODir])
  else
    if Windows.SetTimer(FHWnd, EventIDs[IODir], TimerIntervals[IODir], nil) = 0 then
      Error('SetTimer: '+LastErrMsg);
end;

// закрываем и обнуляем все хэндлы, за исключением Id процесса (Id просто обнуляем - это не хэндл)
procedure TConsoleEmulator.ClearHandles;
var h: THandles;
begin
  for h := Low(THandles) to High(THandles) do
    CloseAndZeroHandle(FHandles[h]);
  FProcessId := 0;
end;

// завершаем процесс и обнуляем все хэндлы
procedure TConsoleEmulator.Terminate(ByTimeout: Boolean);
begin
  if FProcessId <> 0 then
  begin
    // if the input pipe is open, try closing it, child process will probably flush its STDOUT
    if ByTimeout then
      if FHandles[ehStdIn] <> 0 then
      begin
        CloseInput;
        Sleep(100); // wait some time to flush
        TryRead;    // read the data
      end;
    KillProcessTree(FProcessId);
    FProcExitCode := 0;
    if ByTimeout
      then SetState(cesTerminatedByTimeout)
      else SetState(cesTerminated);
  end;
  ClearHandles;
end;

// Execute console application or script
//   CmdLine              - command to execute
//   CurrDir  [opt]       - current dir for command
//   EnvVars  [opt]       - custom environment variables, "v1=val1;v2=val2;..."
//   InputStm [opt]       - stream to write to STDIN
//   AutoCloseInput [opt] - close STDIN pipe when InputStm reaches the end. See Notes#WAIT FOR INPUT
procedure TConsoleEmulator.Launch(CmdLine: string; CurrDir: string; EnvVars: string;
                                  InputStm: TStream; AutoCloseInput: Boolean);
var si: TStartupInfo;
    pi: TProcessInformation;
    sa: TSecurityAttributes;
    pOldEnv, tmp: PChar;
    OldEnvLen, NewEnvLen, err: Integer;
    hStdOut, hStdIn: THandle;
    mode: DWORD;
begin
  if FState = cesRunning then Error(S_AlreadyLaunched);
  FProcExitCode := 0;

  // обнуляем все переменные, чтобы в finally их все скопом закрыть
  ClearHandles;
  hStdOut := 0; hStdIn := 0;
  ZeroMem(si, SizeOf(si));
  ZeroMem(pi, SizeOf(pi));

  try try
    // TSecurityAttributes для процесса и труб
    ZeroMem(sa, SizeOf(sa));
    sa.nLength := SizeOf(sa);
    sa.lpSecurityDescriptor := nil;
    sa.bInheritHandle := True;
    // create pipes
    mode := PIPE_READMODE_BYTE or PIPE_NOWAIT;
    // STDOUT
    if not CreatePipe(FHandles[ehStdOut], hStdOut, @sa, 1) then
      Error('CreatePipe: '+LastErrMsg);
    // Ensure the read handle to the pipe for STDOUT is not inherited (from MSDN example)
    SetHandleInformation(FHandles[ehStdOut], HANDLE_FLAG_INHERIT, 0);
    // Set non-blocking R/W mode for the pipe (!)
    SetNamedPipeHandleState(FHandles[ehStdOut], mode, nil, nil);
    // STDIN
    if not CreatePipe(hStdIn, FHandles[ehStdIn], @sa, 1) then
      Error('CreatePipe: '+LastErrMsg);
    // Ensure the write handle to the pipe for STDIN is not inherited (from MSDN example)
    SetHandleInformation(FHandles[ehStdIn], HANDLE_FLAG_INHERIT, 0);
    // Set non-blocking R/W mode for the pipe (!)
    SetNamedPipeHandleState(FHandles[ehStdIn], mode, nil, nil);

    // заполняем структуры для создания процесса
    si.cb          := SizeOf(si);
    si.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    si.wShowWindow := SW_HIDE;
    si.hStdInput   := hStdIn;
    si.hStdOutput  := hStdOut;
    si.hStdError   := hStdOut;
    if CurrDir = '' then CurrDir := GetCurrentDir;

    // Конструируем новое окружение из переданной в параметре строки, сразу добавляя
    // и сигнатуру сообщения об ошибке. Соответственно EnvVars у нас всегда непуст,
    // и проверки if (EnvVars <> '') можно убрать
    EnvVars := ErrSignName + '=' + ErrSign + ';' + EnvVars;
    if EnvVars[Length(EnvVars)] <> ';' then EnvVars := EnvVars + ';';
    EnvVars := StringReplace(EnvVars, ';', #0, [rfReplaceAll]);
    NewEnvLen := Length(EnvVars);

    // Получаем старые переменные окружения, определяем их длину
    // (НЕ включая завершающий второй ноль, чтобы обработать случай пустой строки)
    pOldEnv := GetEnvironmentStrings;
    if pOldEnv <> nil then
    begin
      tmp := pOldEnv;
      while not ( (tmp^ = #0) and (CharNext(tmp)^ = #0) ) do Inc(tmp, StrLen(tmp)+1);
      OldEnvLen := tmp - pOldEnv;
    end
    else
      OldEnvLen := 0;

    // Собираем новый список переменных окружения, в конец вручную добавляем второй ноль
    SetLength(EnvVars, NewEnvLen + OldEnvLen + 1);
    Move(pOldEnv^, EnvVars[NewEnvLen + 1], OldEnvLen*SizeOf(Char));
    EnvVars[Length(EnvVars)] := #0;
    FreeEnvironmentStrings(pOldEnv);

    FCmdLine := CmdLine; // обеспечиваем изменяемость комстроки - особенности CreateProcessW
    if not CreateProcess(nil, PChar(FCmdLine), @sa, nil, True,
                         CREATE_NEW_CONSOLE{$IFDEF UNICODE} or CREATE_UNICODE_ENVIRONMENT{$ENDIF},
                         PChar(EnvVars), PChar(CurrDir), si, pi) then
    begin
      err := GetLastError;
      Error('CreateProcess: '+SysErrorMessage(err)+' ['+IntToStr(err)+'], "'+CmdLine+'"');
    end;
    FProcessId := pi.dwProcessId;
    FHandles[ehProcess] := pi.hProcess;

    // присваиваем различные внутренние поля, запускаем таймеры и меняем состояние
    FLaunchTick := GetTickCount; FLastActiveTick := GetTickCount;

    SetTimer(ioOutput, True);
    // set input stream and activate input timer
    FInputStm := InputStm;
    fAutoCloseInput := AutoCloseInput;
    if FInputStm <> nil then
    begin
      SetTimer(ioInput, True);
      SetLength(FInputBuf, BufSize);
    end
    else
      SetLength(FInputBuf, 0);

    SetState(cesRunning);
  except
    on E: Exception do
    begin
      Terminate;
      raise;
    end;
  end;
  finally
    // освобождаем хэндлы потока и уже унаследованных концов труб
    CloseAndZeroHandle(hStdIn);
    CloseAndZeroHandle(hStdOut);
    CloseAndZeroHandle(pi.hThread);
  end;
end;

procedure TConsoleEmulator.CloseInput;
begin
  CloseAndZeroHandle(FHandles[ehStdIn]);
end;

procedure TConsoleEmulator.TryRead;
var
  bytes, StartTick: Cardinal;
  res: Boolean;
begin
  // read data from pipe in a loop
  if FHandles[ehStdOut] <> 0 then
  begin
    StartTick := GetTickCount;
    repeat
      // read data, on error close the {}handle and break the loop
      res := ReadFile(FHandles[ehStdOut], FOutputBuf[0], Length(FOutputBuf), bytes, nil);
      if not res then
        case GetLastError of
          ERROR_NO_DATA:      // here: pipe is currently empty, that's OK
            ;
          ERROR_BROKEN_PIPE:  // pipe closed on the other end
            CloseAndZeroHandle(FHandles[ehStdOut]);
          else                // other error - report & close
          begin
            if Assigned(OnIOError) then
              OnIOError(Self, ioOutput, GetLastError);
            CloseAndZeroHandle(FHandles[ehStdOut]);
          end;
        end; // case
      if bytes = 0 then Break;

      // if something was read, regardless the error, process it
      FLastActiveTick := GetTickCount;
      if Assigned(OnDataOutput) then OnDataOutput(Self, @FOutputBuf[0], bytes);
      if Assigned(FOutputStm)   then FOutputStm.Write(FOutputBuf[0], bytes);
//debug(timetostr(now)+' '+'read '+itos(bytes));

      if not res then Break; // if ReadFile failed, break
      // control loop execution time
      if TicksSince(StartTick) > TimerIntervals[ioOutput] div 2 then Break;
    until False;
  end;
end;

procedure TConsoleEmulator.TryWrite;
var
  InputPtr: PByte;
  ToWrite: Integer;
  bytes, StartTick: Cardinal;
  res: Boolean;
begin
  if FProcessId = 0 then Exit;
  if FInputStm = nil then Exit;
  if FHandles[ehStdIn] <> 0 then
  begin
    // copy input data from stream to buffer
    ToWrite := FInputStm.Read(FInputBuf[0], Length(FInputBuf));
    if ToWrite <= 0 then // nothing to write
    begin
      if FAutoCloseInput then
      begin
        CloseInput;
        SetTimer(ioInput, False);
      end;
      Exit;
    end;
    InputPtr := @FInputBuf[0];
    StartTick := GetTickCount;
    // write data in a loop
    repeat
      res := WriteFile(FHandles[ehStdIn], InputPtr^, ToWrite, bytes, nil);
      if not res then
        case GetLastError of
          ERROR_NO_DATA,      // here: pipe closed
          ERROR_BROKEN_PIPE:  // pipe closed on the other end
            CloseAndZeroHandle(FHandles[ehStdIn]);
          else                // other error - report & close
          begin
            if Assigned(OnIOError) then
              OnIOError(Self, ioInput, GetLastError);
            CloseAndZeroHandle(FHandles[ehStdIn]);
          end;
        end; // case
      if bytes = 0 then Break;

      // if something was read, regardless the error, process it
      FLastActiveTick := GetTickCount;
      Inc(InputPtr, bytes);
      Dec(ToWrite, bytes);

      if not res then Break; // if WriteFile failed, break
      // control loop execution time
      if TicksSince(StartTick) > TimerIntervals[ioInput] div 2 then Break;
    until False;
    // ToWrite is amount of data unwritten so rewind the stream (hoping it supports that!)
    if ToWrite > 0 then
      FInputStm.Seek(-ToWrite, soCurrent);
  end;
//debug(timetostr(now)+' '+itos(FInputStm.Position));
end;

// по таймеру периодически проверять состояние запущенного процесса
// и считывать выводимую в трубу информацию

function TConsoleEmulator.WndProc(wnd: HWND; msg: UINT; wPar: WPARAM; lPar: LPARAM): LRESULT;
begin
  Result := 0;
  if (msg = WM_TIMER) and (FState = cesRunning) then
  begin
    if wPar = EventIDs[ioInput] then // write data from stream (if assigned) to the ehProcess input pipe
    begin
      TryWrite;
    end
    else if wPar = EventIDs[ioOutput] then       // read data from ehProcess
    begin
      TryRead;
      // проверяем, не завершился ли ещё процесс, и если так, то меняем состояние
      if FProcessId = 0 then Exit;
      if (not GetExitCodeProcess(FHandles[ehProcess], FProcExitCode)) or (FProcExitCode <> STILL_ACTIVE) then
        begin ClearHandles; SetState(cesFinished); end
      // если нет - проверяем, не истёк ли таймаут неактивности
      else if FTimeout <> 0 then
        if TicksSince(FLastActiveTick) >= FTimeout*MSecsPerSec then
          Terminate(True);
    end // Output
    else Exit;
  end // msg = WM_TIMER
  else
    Result := DefWindowProc(wnd, msg, wPar, lPar);
end;

// Write the data to input pipe
function TConsoleEmulator.DataInput(const Data; DataLen: Integer): Integer;
var bytes: Cardinal;
begin
  if not WriteFile(FHandles[ehStdIn], Data, DataLen, bytes, nil) then
    Result := -1
  else
    Result := bytes;
end;

// посылка команды запущенному процессу через перехваченную трубу
procedure TConsoleEmulator.SendCmd(const Cmd: string);
var bytes : Integer;
    curr  : Integer;
    CmdToSend: AnsiString;
begin
  // выполнять, только если все нужные хэндлы ненулевые
  if (FProcessId = 0) or (FHandles[ehStdIn] = 0) or (FHandles[ehStdOut] = 0) or
     (FState <> cesRunning) then Error(S_NotLaunched);
  CmdToSend := AnsiString(Cmd) + NL; // признак конца команды, иначе будет ждать
  curr := 1;
  while curr <= Length(CmdToSend) do
  begin
    bytes := DataInput(CmdToSend[curr], StrSize(CmdToSend)-curr+1);
    if bytes = -1 then Error('WriteFile: '+LastErrMsg);
    if bytes > 0 then Inc(curr, bytes) else Break; {}// repeat
  end;
end;

{$ENDREGION}

{$REGION 'TfrmConsole'}

{$IFDEF ConEm_VCL}

const MaxMemoLines = 500;

constructor TfrmConsole.Create(AOwner: TComponent; ATimeout: Cardinal);
begin
  CreateNew(AOwner);
  // init внутренние поля
  FMemoLineCompl := True;
  FThisLineCompl := False;
  FConsEmul := TConsoleEmulator.Create(ATimeout);
  FConsEmul.OnDataOutput := ConsEmulOnDataOutput;
  FConsEmul.OnStateChange := ConsEmulOnStateChange;
  // init себя
  Caption := S_FormCaption;
  ClientHeight := 370;
  ClientWidth := 730;
  Font.Size := 9;
  Font.Name := 'Tahoma';
  KeyPreview := True;
  Padding.Left := 5;
  Padding.Top := 5;
  Padding.Right := 5;
  Padding.Bottom := 5;
  Position := poDesktopCenter;
  OnKeyUp := FormKeyUp;
  // init контролы
  with TLabel.Create(Self) do
  begin;
    Parent := Self;
    SetBounds(8, 361, 17, 25);
    Anchors := [akLeft, akBottom];
    Caption := '>';
    Font.Size := 16;
    Font.Name := 'Tahoma';
    Font.Style := [fsBold];
  end;
  lblState := TLabel.Create(Self);
  with lblState do
  begin
    Parent := Self;
    SetBounds(8, 340, 232, 14);
    Anchors := [akLeft, akBottom];
    AutoSize := True;
  end;
  lblEmulCallback := TLabel.Create(Self);
  with lblEmulCallback do
  begin
    Parent := Self;
    top:=0;
    Align := alTop;
    AlignWithMargins := True;
    Margins.SetBounds(5,5,5,5);
    Caption := '';
  end;
  eCommand := TEdit.Create(Self);
  with eCommand do
  begin
    Parent := Self;
    SetBounds(31, 366, 500, 22);
    Anchors := [akLeft, akRight, akBottom];
  end;
  with TButton.Create(Self) do
  begin
    Parent := Self;
    SetBounds(545, 361, 95, 32);
    Anchors := [akRight, akBottom];
    Caption := S_BtnSendCaption;
    Default := True;
    OnClick := btnSendCmdClick;
  end;
  with TButton.Create(Self) do
  begin
    Parent := Self;
    SetBounds(645, 361, 90, 32);
    Anchors := [akRight, akBottom];
    Caption := S_BtnAbortCaption;
    OnClick := btnAbortClick;
  end;
  mConsole := TMemo.Create(Self);
  with mConsole do
  begin
    Parent := Self;
    Height := 305;
    Align := alTop;
    Anchors := [akLeft, akTop, akRight, akBottom];
    Color := clCream;
    Font.Size := 10;
    Font.Name := 'Courier';
    ReadOnly := True;
    ScrollBars := ssVertical;
    WordWrap := False;
  end;
end;

destructor TfrmConsole.Destroy;
begin
  FreeAndNil(FConsEmul);
  FLogFile := nil;
  inherited;
end;

function TfrmConsole.GetLogFileName: string;
begin
  if FLogFile = nil then Result := '' else Result := FLogFile.FileName;
end;

procedure TfrmConsole.SetLogMaxFileSize(Val: Int64);
begin
  if FLogFile <> nil then
    FLogFile.SetRotateOptions(rmAfterClose, raRenamePatt, Val, '"%0s"_yy-mm-dd', '~%.2d');
end;

procedure TfrmConsole.btnAbortClick(Sender: TObject);
begin
  FConsEmul.Terminate;
end;

procedure TfrmConsole.btnSendCmdClick(Sender: TObject);
begin
  FConsEmul.SendCmd(eCommand.Text);
  eCommand.SelectAll;
end;

// Поступили данные в консоль - пишем их в мемо и в лог-файл
procedure TfrmConsole.ConsEmulOnDataOutput(Sender: TConsoleEmulator; Data: PByte; DataLen: Integer);
var tmp: String;
    pData, pNl: PAnsiChar;
begin
  try
  pData := PAnsiChar(Data);
  OemToAnsiBuff(pData, pData, DataLen);
  if FLogFile <> nil then
    FLogFile.WriteData(PByte(pData), DataLen);
  mConsole.Lines.BeginUpdate;
  // если строк в мемо больше максимума, удаляем их
  if mConsole.Lines.Count > MaxMemoLines + 50 then
    while mConsole.Lines.Count > MaxMemoLines do mConsole.Lines.Delete(0);
  // цикл, пока не исчерпаем все данные
  while DataLen > 0 do
  begin
    // пропускаем все #10
    while (DataLen > 0) and (pData^ = #10) do
      begin Inc(pData); Dec(DataLen); end;
    // ищем #13, если не нашли - присваиваем ему указатель на конец строки
    pNl := AnsiStrScan(pData, #13);
    FThisLineCompl := pNl <> nil;
    if pNl = nil then pNl := PAnsiChar(pData+DataLen);
    // получаем строку и в зависимости от флага прибавляем к существующей строке
    // в мемо либо добавляем новую строку
    tmp := string(Copy(pData, 1, pNl-pData));
    with mConsole.Lines do
      if FMemoLineCompl
        then Add(tmp)
        else Strings[Count-1] := Strings[Count-1] + tmp;
    FMemoLineCompl := FThisLineCompl;
    Dec(DataLen, pNl-pData+1);
    Inc(pData, pNl-pData+1);
    if StrIsStartingFrom(tmp,ErrSign) then
      FErrorMsg := tmp;
  end;
  finally
    mConsole.Lines.EndUpdate;
    SendMessage(mConsole.{}handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;
end;

// Процесс завершён - сообщаем об этом
procedure TfrmConsole.ConsEmulOnStateChange(Sender: TConsoleEmulator; State: TConsEmulState; ExitCode: Cardinal);
var ExitStr: string;
begin
  lblState.Caption := Format(S_LblStatePatt, [S_ConsEmulStateLabels[State], Sender.CmdLine]);
  case State of
    cesWaiting:
      begin
        FExitCode := 0; FErrorMsg := '';
      end;
    cesRunning:
      begin
        Log(Format(S_ProcessStatePatt, [DateTimeToStr(Now), Sender.CmdLine, ' ' + S_ConsEmulStateLabels[State]]));
        FExitCode := 0; FErrorMsg := '';
      end;
    cesFinished, cesTerminated, cesTerminatedByTimeout:
      begin
        FExitCode := ExitCode;
        if FErrorMsg <> ''
          then ExitStr := Format(S_ErrMsgPatt, [FErrorMsg])
          else ExitStr := '';
        // код процесса имеет смысл только при нормальном завершении
        if State = cesFinished then
          ExitStr := Format(S_ExitCodePatt, [FExitCode, ExitStr]);

        Log(Format(S_ProcessStatePatt, [DateTimeToStr(Now), '', S_ConsEmulStateLabels[State] + ExitStr]));
        lblState.Caption := lblState.Caption + ExitStr;
        if FLogFile <> nil then
          FLogFile.Close; // закрываем файл лога после выполнения команды - чтобы не держать
        if Assigned(OnProcessFinished) then OnProcessFinished(Self, State);
      end;
  end;
end;

// Скрытие формы по Escape
procedure TfrmConsole.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

// Запуск приложения в консоли
function TfrmConsole.Launch(const CmdLine: string; LogFN: string; CurrDir: string; EnvVars: string): Boolean;
begin
  Result := False;
  try
    // переоткрываем файл либо закрываем и открываем другой
    FLogFile := FileWriter(LogFN);
    FConsEmul.Launch(CmdLine, CurrDir, EnvVars);
    Result := True;
  except
    on E: Exception do begin
      Log(Format(S_ErrLaunching, [CmdLine, E.Message]));
      lblState.Caption := Format(S_LblStatePatt, [S_ConsEmulStateLabels[FConsEmul.State], E.Message]);
      FErrorMsg := E.Message;
    end;
  end;
end;

// Запись строки в мемо и в файл
procedure TfrmConsole.Log(Msg: string);
var MsgAnsi: AnsiString;
begin
  // добавляем перевод строки, чтобы не склеивались
  if LeftStr(Msg, 2) <> NL then
    Msg := Msg + NL;
  mConsole.Lines.Add(Msg);
  MsgAnsi := AnsiString(NL+Msg);
  if FLogFile <> nil then
    FLogFile.WriteData(PByte(@MsgAnsi[1]), Length(MsgAnsi));
end;

function TfrmConsole.IsRunning: Boolean;
begin
  Result := (FConsEmul.State=cesRunning);
end;

{$ENDIF}

{$ENDREGION}

end.
