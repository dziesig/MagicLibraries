unit magicmainformbase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, ComCtrls,

  Persists1, CursorStackUnit1, MagicFormFrame1;

type

  { TMagicMainFormBase }

  TMagicMainFormBase = class(TForm)
    FilePreferencesAction: TAction;
    PrimaryFrame: TFrame1;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem9: TMenuItem;
    FilePrintAction: TAction;
    FilePrinterSetupAction: TAction;
    FileExitAction: TAction;
    FileSaveAsAction: TAction;
    FileSaveAction: TAction;
    FileOpenAction: TAction;
    FileNewAction: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure FileExitActionExecute(Sender: TObject);
    procedure FileNewActionExecute(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure FilePreferencesActionExecute(Sender: TObject);
    procedure FilePrintActionExecute(Sender: TObject);
    procedure FilePrinterSetupActionExecute(Sender: TObject);
    procedure FileSaveActionExecute(Sender: TObject);
    procedure FileSaveAsActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    fCurrentFile: String;

    procedure SetCurrentFile(AValue: String);
    procedure SetData(AValue: TPersists);
    { private declarations }
  protected
    vAppName     : String;
    vDataName    : String;
    vDefaultExt  : String;
    vTitleLeader : String;
    vFileFilter  : String;

    fData : TPersists;

    CursorStack : TFormCursorStack;

    procedure UpdateData; virtual; abstract; // so we can get the last control data
    procedure FileNew; virtual;
    procedure FileOpen( FileName : String ); virtual; abstract;
    procedure FileSave; virtual; abstract;

  public
    { public declarations }

    property CurrentFile : String    read fCurrentFile write SetCurrentFile;
    property Data        : TPersists read fData        write SetData;

  end;

var
  MagicMainFormBase: TMagicMainFormBase;

const
  NoFile = 'NoFILE';

implementation

{$R *.lfm}

uses
  Common1;

{ TMagicMainFormBase }

procedure TMagicMainFormBase.FileNewActionExecute(Sender: TObject);
var
  Ans : Integer;
begin
  UpdateData;
  if Data.Modified then
     begin
       Ans := MessageDlg( CurrentFile + ' has been modified.'#13#10 +
                          'Do you want to save it before creating a new one?',
                          mtConfirmation, [mbYes, mbNo, mbCancel], 0);
       case Ans of
         mrYes:
           begin
             FileSaveActionExecute( Sender );
             FileNew;
           end;
         mrNo:
           FileNew;
         mrCancel: ;
       end;
     end
  else
    FileNew;
end;

procedure TMagicMainFormBase.FileExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMagicMainFormBase.FileOpenActionExecute(Sender: TObject);
  procedure DoOpen;
  begin
    OpenDialog1.InitialDir := DefaultSaveLocation;
    OpenDialog1.DefaultExt := vDefaultExt;
    OpenDialog1.Filter := vFileFilter;
    if OpenDialog1.Execute then
      begin
        CurrentFile := OpenDialog1.FileName;
        FileOpen( CurrentFile );
      end;
  end;
var
  Ans : Integer;
begin
  UpdateData;
  if Data.Modified then
     begin
       Ans := MessageDlg( CurrentFile + ' has been modified.'#13#10 +
                          'Do you want to save it before opening another one?',
                          mtConfirmation, [mbYes, mbNo, mbCancel], 0);
       case Ans of
         mrYes:
           begin
             FileSaveActionExecute( Sender );
             DoOpen;
           end;
         mrNo:
           DoOpen;
         mrCancel: ;
       end;
     end
  else
    DoOpen;
end;

procedure TMagicMainFormBase.FilePreferencesActionExecute(Sender: TObject);
begin

end;

procedure TMagicMainFormBase.FilePrintActionExecute(Sender: TObject);
begin

end;

procedure TMagicMainFormBase.FilePrinterSetupActionExecute(Sender: TObject);
begin

end;

procedure TMagicMainFormBase.FileSaveActionExecute(Sender: TObject);
begin
  if CurrentFile = NoFile then
     FileSaveAsActionExecute(Sender)
  else
    begin
      UpdateData;
      FileSave;
    end;
end;

procedure TMagicMainFormBase.FileSaveAsActionExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := DefaultSaveLocation;
  SaveDialog1.DefaultExt := vDefaultExt;
  SaveDialog1.Filter     := vFileFilter;
  UpdateData;
  if SaveDialog1.Execute then
    begin
      CurrentFile := SaveDialog1.FileName;
      FileSave;
    end;
end;

procedure TMagicMainFormBase.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  Ans : Integer;
begin
  CursorStack.Push( crHourGlass );
  UpdateData;
  CursorStack.Pop;
  if Data.Modified then
    begin
      Ans := MessageDlg( CurrentFile + ' has been modified'#13#10 +
                         'Do you want to save it before exiting?',
                         mtConfirmation,
                         [mbYes, mbNo, mbCancel], 0 );

      case Ans of
        mrYes:
          begin
            FileSaveActionExecute( Sender );
            CanClose := False;
          end;
        mrNo:
          CanClose := True;
        mrCancel:
          CanClose := False;
      end;
    end
  else
    CanClose := True;

end;

procedure TMagicMainFormBase.FormCreate(Sender: TObject);
begin
  CursorStack := TFormCursorStack.Create( self );
end;

//procedure TMagicMainFormBase.FormResize(Sender: TObject);
//begin
////  PrimaryFrame.OnAfterFormChanged := @ResizeFrame;
////  PrimaryFrame.Height := ClientHeight;
//end;

procedure TMagicMainFormBase.SetCurrentFile(AValue: String);
begin
  if fCurrentFile=AValue then Exit;
  fCurrentFile:=AValue;
  StatusBar1.Panels[1].Text := fCurrentFile;
end;

procedure TMagicMainFormBase.SetData(AValue: TPersists);
begin
  Caption := vAppName + ' - ' + aValue.Name;
  fData:=AValue;
end;

procedure TMagicMainFormBase.FileNew;
begin
  CurrentFile := NoFile;
end;


end.

