object dmMain: TdmMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 519
  Width = 806
  object clDownload: TclDownLoader
    InternetAgent = 'Mozilla/4.0 (compatible; Clever Internet Suite)'
    Left = 32
    Top = 24
  end
  object alMain: TActionList
    Left = 32
    Top = 96
    object acScan: TAction
      Category = 'Downloads'
      Caption = '&Scan'
      OnUpdate = acScanUpdate
    end
    object acExit: TAction
      Caption = 'E&xit'
    end
  end
  object mnuMain: TMainMenu
    Left = 88
    Top = 96
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
end
