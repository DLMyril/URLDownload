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
    procedure lePatternChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateDisplay;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.lePatternChange(Sender: TObject);
begin
  dmMain.SearchPattern := lePattern.Text;
end;

procedure TfrmMain.UpdateDisplay;
begin
  // update display based on classes in MainData
end;

end.
