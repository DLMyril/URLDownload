unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MainData, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TfrmMain = class(TForm)
    mmoUrl: TMemo;
    btnStart: TButton;
    lvResult: TListView;
    lePattern: TLabeledEdit;
    btnSaveResult: TButton;
    procedure lePatternChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoUrlChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateDisplay;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses URLFileUnit;

var
  Manager: TURLFileManager;
                 
{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Manager := TURLFileManager.GetInstance;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TfrmMain.lePatternChange(Sender: TObject);
begin
  dmMain.SearchPattern := lePattern.Text;
end;

procedure TfrmMain.mmoUrlChange(Sender: TObject);
begin
  dmMain.UrlList.Text := mmoUrl.Lines.Text;
end;

procedure TfrmMain.UpdateDisplay;
var
  i: integer;
  li: TListItem;
  fi: TUrlFileInfo;
begin
  dmMain.UrlList.Text := mmoUrl.Lines.Text;
  lvResult.Items.BeginUpdate;
  lvResult.Clear;
  for i := 0 to (mmoUrl.Lines.Count - 1) do begin
    li := lvResult.Items.Add;
    fi := Manager.FileStatus[mmoUrl.Lines[i]];
    if assigned(fi) then begin
      li.Caption := mmoUrl.Lines[i];
      li.SubItems.Add(UrlFileStatusStr[fi.Status]);
      li.SubItems.Add(fi.Pattern);
      li.SubItems.Add(IntToStr(fi.Count));
    end;
  end;
  lvResult.Items.EndUpdate;
  lvResult.Refresh;
end;                         

end.
