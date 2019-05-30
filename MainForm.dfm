object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'URL Downloader'
  ClientHeight = 571
  ClientWidth = 936
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = dmMain.mnuMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object mmoUrl: TMemo
    Left = 8
    Top = 32
    Width = 345
    Height = 457
    Lines.Strings = (
      'C:\Proj\UrlDownload\Test1.html'
      'C:\Proj\UrlDownload\Test2.html'
      'C:\Proj\UrlDownload\Test3.html')
    TabOrder = 0
    OnChange = mmoUrlChange
  end
  object btnStart: TButton
    Left = 96
    Top = 495
    Width = 161
    Height = 25
    Action = dmMain.acScan
    TabOrder = 1
  end
  object lvResult: TListView
    Left = 359
    Top = 8
    Width = 554
    Height = 481
    Columns = <
      item
        AutoSize = True
        Caption = 'URL'
      end
      item
        Alignment = taCenter
        AutoSize = True
        Caption = 'Status'
      end
      item
        AutoSize = True
        Caption = 'Pattern'
      end
      item
        Alignment = taRightJustify
        AutoSize = True
        Caption = 'Pattern Count'
      end>
    Items.ItemData = {
      05DE0000000300000000000000FFFFFFFFFFFFFFFF02000000FFFFFFFF000000
      0007550052004C002F004F006E0065000650006100750073006500640088DDC4
      22012D00D0E0C42200000000FFFFFFFFFFFFFFFF02000000FFFFFFFF00000000
      07550052004C002F00540077006F0008460069006E0069007300680065006400
      E0CEC422013400D8DEC42200000000FFFFFFFFFFFFFFFF02000000FFFFFFFF00
      00000009550052004C002F00540068007200650065000B44006F0077006E006C
      006F006100640069006E00670000DCC422012D0010D8C422FFFFFFFFFFFFFFFF
      FFFFFFFF}
    TabOrder = 2
    ViewStyle = vsReport
  end
  object lePattern: TLabeledEdit
    Left = 480
    Top = 497
    Width = 121
    Height = 21
    EditLabel.Width = 72
    EditLabel.Height = 13
    EditLabel.Caption = 'Search Pattern'
    LabelPosition = lpLeft
    LabelSpacing = 8
    TabOrder = 3
    Text = 'div'
    OnChange = lePatternChange
  end
  object btnSaveResult: TButton
    Left = 672
    Top = 495
    Width = 177
    Height = 25
    Action = dmMain.acSaveResult
    TabOrder = 4
  end
end
