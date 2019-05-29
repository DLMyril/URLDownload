program UrlDownload;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  MainData in 'MainData.pas' {dmMain: TDataModule},
  URLFileUnit in 'URLFileUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmMain, dmMain);
  Application.Run;
end.
