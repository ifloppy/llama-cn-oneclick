unit launch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FileUtil, process;

type

  { TFormLaunch }

  TFormLaunch = class(TForm)
    Button1: TButton;
    btnSave: TButton;
    ShowCommandline: TEdit;
    inputModelFile: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    inputPrompt: TMemo;
    Process1: TProcess;
    rbConsole: TRadioButton;
    rbGUI: TRadioButton;
    RadioGroup1: TRadioGroup;
    procedure btnSaveClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormLaunch: TFormLaunch;

implementation

uses commonunit;

{$R *.lfm}

{ TFormLaunch }

procedure TFormLaunch.Button1Click(Sender: TObject);
begin
  if Process1.Running then begin
    ShowMessage('已经有一个实例正在运行，暂不支持同时运行多个实例');
    exit;
  end;

  //This is a args generator
  Process1.Parameters.Clear;

  //shared args
  //model
  Process1.Parameters.Add('-m');
  Process1.Parameters.Add(GetCurrentDir+'\models\'+inputModelFile.Caption);

  //prompt
  Process1.Parameters.Add('-p');
  Process1.Parameters.Add(QuotedStr(inputPrompt.Text));

  if rbConsole.Checked then begin
    //Use console mode
    Process1.Parameters.Add('--color');
    Process1.Parameters.Add('-i');
  end;

  Process1.Execute;
  Process1.Parameters.StrictDelimiter:=true;
  Process1.Parameters.Delimiter:=' ';
  ShowCommandline.Text:=Process1.Executable+' '+Process1.Parameters.DelimitedText;
end;

procedure TFormLaunch.btnSaveClick(Sender: TObject);
begin
  config.WriteString('launch', 'prompt', inputPrompt.Text);
  config.WriteString('launch', 'model', inputModelFile.Text);
end;

procedure TFormLaunch.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TFormLaunch.FormCreate(Sender: TObject);
var
  F: TSearchRec;
  s: string;
begin
  inputPrompt.Lines.Text:=config.ReadString('launch', 'prompt', defaultPrompt);
  inputModelFile.Text:=config.ReadString('launch', 'model', '');

  if FindFirst(GetCurrentDir+'\models\*.*', faAnyFile, F) = 0 then begin
    repeat
      with F do begin
        s:=ExtractFileName(F.Name);
        case s of
          //Without common files that are not model files
          'README.md': Break;
          '.gitignore': Break;
          '.gitattributes': Break;
          '.': Break;
          '..': Break;
          '.git': Break;
        end;
        inputModelFile.Items.Add(s);
      end;
    until FindNext(F) <> 0;
  end;
  FindClose(F);

  Process1.Executable:=GetCurrentDir+'\llama\main.exe';
end;

end.

