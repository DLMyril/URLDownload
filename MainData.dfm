object dmMain: TdmMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 519
  Width = 806
  object alMain: TActionList
    Left = 24
    Top = 16
    object acScan: TAction
      Category = 'Downloads'
      Caption = '&Scan'
      OnExecute = acScanExecute
      OnUpdate = acScanUpdate
    end
    object acExit: TAction
      Category = 'Files'
      Caption = 'E&xit'
      OnExecute = acExitExecute
    end
    object acSaveResult: TAction
      Category = 'Files'
      Caption = 'Save &Results'
      OnExecute = acSaveResultExecute
    end
  end
  object mnuMain: TMainMenu
    Left = 80
    Top = 16
    object miDownloads: TMenuItem
      Caption = '&Downloads'
      object miDownloadStart: TMenuItem
        Action = acScan
      end
    end
    object miExit: TMenuItem
      Action = acExit
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.csv'
    FileName = 'DownloadResult.csv'
    Left = 24
    Top = 72
  end
end
