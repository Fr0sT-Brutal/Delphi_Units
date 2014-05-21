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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  ConEm := TfrmConsole.Create(Self, 3000);
  Memo1.Lines.Delimiter := ';';
end;

end.
