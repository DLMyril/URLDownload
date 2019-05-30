unit MainData;

interface

uses
  System.SysUtils, System.Classes, Vcl.Menus, System.Actions, Vcl.ActnList, clMultiDC, clSingleDC;

type
  TdmMain = class(TDataModule)
    alMain: TActionList;
    acScan: TAction;
    mnuMain: TMainMenu;
    miDownloads: TMenuItem;
    miDownloadStart: TMenuItem;
    acExit: TAction;
    miExit: TMenuItem;
    procedure acScanUpdate(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fSearchPattern: string;
    HasChanged: boolean;
    procedure SetSearchPattern(const Value: string);
  public
    UrlList: TStringList;
    property SearchPattern: string read fSearchPattern write SetSearchPattern;
    procedure UpdateDisplay;
    procedure StartDownload;
  end;

var
  dmMain: TdmMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses MainForm, URLFileUnit;

var
  Manager: TURLFileManager;

procedure TdmMain.acScanUpdate(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  SearchPattern := 'div'; //default requested
  HasChanged := false;
  UrlList := TStringList.Create;
  Manager := TUrlFileManager.GetInstance;
end;                    

procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
  if assigned(UrlList) then UrlList.Free;
end;

procedure TdmMain.SetSearchPattern(const Value: string);
begin
  if fSearchPattern <> Value then begin
    fSearchPattern := Value;
  end;
end;

procedure TdmMain.StartDownload;
var
  i: integer;
  FileInfo: TURLFileInfo;
begin
  for i := 0 to (UrlList.Count-1) do begin
    FileInfo := TURLFileInfo.Create(UrlList[i], 
                ExtractFilePath(ParamStr(0)) + IntToStr(i) + '_' + ExtractFileName(UrlList[i]), 
                SearchPattern);
    Manager.AddToDownload(FileInfo);
  end;
  HasChanged := True;
  UpdateDisplay;
end;

procedure TdmMain.UpdateDisplay;
begin
  // call the update functionality in the gui only if changed
  if HasChanged then
     frmMain.UpdateDisplay;
end;

end.
