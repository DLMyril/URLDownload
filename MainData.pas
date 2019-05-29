unit MainData;

interface

uses
  System.SysUtils, System.Classes, Vcl.Menus, System.Actions, Vcl.ActnList, clMultiDC, clSingleDC,
  clDownLoader;

type
  TdmMain = class(TDataModule)
    clDownload: TclDownLoader;
    alMain: TActionList;
    acScan: TAction;
    mnuMain: TMainMenu;
    miDownloads: TMenuItem;
    miDownloadStart: TMenuItem;
    acExit: TAction;
    miExit: TMenuItem;
    procedure acScanUpdate(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    fSearchPattern: string;
    HasChanged: boolean;
    procedure SetSearchPattern(const Value: string);
  public
    property SearchPattern: string read fSearchPattern write SetSearchPattern;
    procedure UpdateDisplay;
    procedure CalculateSearch;
  end;

var
  dmMain: TdmMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses MainForm;

procedure TdmMain.acScanUpdate(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TdmMain.CalculateSearch;
begin
  // count the number of patterns in each file.
end;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  SearchPattern := 'div'; //default requested
  HasChanged := false;
end;                    

procedure TdmMain.SetSearchPattern(const Value: string);
begin
  if fSearchPattern <> Value then begin
    fSearchPattern := Value;
    CalculateSearch;
    HasChanged := True;
    UpdateDisplay; // if the pattern changes, display needs updated.
  end;
end;

procedure TdmMain.UpdateDisplay;
begin
  // call the update functionality in the gui only if changed
  if HasChanged then
     frmMain.UpdateDisplay;
end;

end.
