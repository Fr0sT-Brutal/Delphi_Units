unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  ConsoleEmulator;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Button1: TButton;
    Edit2: TEdit;
    Label3: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ConEm: TfrmConsole;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var env: string;
begin
  ConEm.Show;
  ConEm.Launch(Edit1.Text, Edit2.Text, '', Memo1.Lines.DelimitedText);
end;

procedure TForm1.Button2Click(Sender: TObject);
var cons: TConsoleEmulator;
    stmin, stmout: TMemoryStream;
begin
  cons := TConsoleEmulator.Create(50);
//  cons.OnStateChange := ConsEmulOnStateChange;
//  cons.OnIOError := ConsEmulOnIOError;
  stmin:= TMemoryStream.Create;
  stmout:= TMemoryStream.Create;

  stmin.LoadFromFile('ConsoleEmulator.pas');
  stmin.Position := 0;
  cons.OutputStm := stmout;
  cons.Launch('gzip.exe', '', '', stmin, True);
  while cons.State = cesRunning do
    Application.ProcessMessages;
  ShowMessage('exit code: '+IntToStr(cons.ExitCode));
  stmout.Position := 0;
  ShowMessage('Output size: '+IntToStr(stmout.size));
  if stmout.size > 0 then
    stmout.SaveToFile('test.gz');

  stmin.Free;
  stmout.Free;
  cons.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConEm := TfrmConsole.Create(Self, 3000);
  Memo1.Lines.Delimiter := ';';
end;

end.
