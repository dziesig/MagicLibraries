//Copyright (c) 2012 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of MagicLibrary.
//
//MagicLibrary is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicLibrary is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicLibrary.  If not, see <http://www.gnu.org/licenses/>.


unit Common1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, StdCtrls, ComCtrls;

  procedure ReadBool( var F : TextFile; var Value : Boolean );{ TODO 3 -odonz -cTest : Implement Unit Test }

  function RectToOrigin( Rect : TRect ) : TRect;
  function Equal( R0, R1 : TRect ) : Boolean;

  procedure AngleTextOut(ACanvas: TCanvas; Angle, X, Y: Integer; Str: string);

  function Min( V0, V1 : Double ) : Double; overload;
  function Min( V0, V1 : Integer ) : Integer; overload;
  function Max( V0, V1 : Double ) : Double; overload;
  function Max( V0, V1 : Integer ) : Integer; overload;

  function AreYouSure( Prompt : String; withCancel : Boolean = False ) : Integer;

  procedure SetPositionRG( RG : TRadioGroup; Value : String );
  procedure SetPositionCB( CB : TComboBox; Value : String );

{==============================================================================}
{ File and Directory Support stuff.                                            }
{==============================================================================}

  type
    PBoolean = ^Boolean;

    TProcessFileObject = procedure (       BasePath, RelPath : String;
                                     const SR                : TSearchRec;
                                     const Depth             : Integer;
                                           UserData          : TObject = nil) of object;
    TProcessFile       = procedure (       BasePath, RelPath : String;
                                     const SR                : TSearchRec;
                                     const Depth             : Integer;
                                           UserData          : TObject = nil);

  { TODO 3 -odonz -cTest : Implement Unit Test }
  procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                               Attributes              : Integer;
                               SubDirsFirst                 : Boolean;
                               DoSomething             : TProcessFile;
                               UserData                : TObject = nil;
                               Stop                    : PBoolean = nil ); overload;

  procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                                Attributes              : Integer;
                                SubDirsFirst                 : Boolean;
                                DoSomething             : TProcessFile;
                                UserData                : TObject;
                                Depth                   : Integer;
                                Stop                    : PBoolean = nil ); overload;

  { TODO 3 -odonz -cTest : Implement Unit Test }
  procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                               Attributes              : Integer;
                               SubDirsFirst                 : Boolean;
                               DoSomething             : TProcessFileObject;
                               UserData                : TObject = nil;
                               Stop                    : PBoolean = nil ); overload;

  procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                                Attributes              : Integer;
                                SubDirsFirst                 : Boolean;
                                DoSomething             : TProcessFileObject;
                                UserData                : TObject;
                                Depth                   : Integer;
                                Stop                    : PBoolean = nil ); overload;

  procedure RmFiles(       Base, Rel : String;      { TODO 3 -odonz -cTest : Implement Unit Test }
                     const SR        : TSearchRec;
                     const Depth     : Integer );

  procedure CopyFile(Src, Dst : String);

  function CopyFiles(FromPath, ToPath, Wildcard : String; Force : Boolean = false ) : Boolean;
  function CountFilesOrDirectories( Path : String ) : Integer;

  function RemoveExt( FilePath : String ) : String;

  function BuildDateTime : String;

{==============================================================================}
{ Config (.ini) file support.  Slow but easy (and isn't used that often).      }
{==============================================================================}
  procedure SetConfig( const Section, Ident : String; const Value : String ); overload;
  function  GetConfig( const Section, Ident : String; const Default : String ) : String; overload;
  procedure SetConfig( const Section, Ident : String; const Value : Integer ); overload;
  function  GetConfig( const Section, Ident : String; const Default : Integer ) : Integer; overload;
  procedure SetConfig( const Section, Ident : String; const Value : Double ); overload;
  function  GetConfig( const Section, Ident : String; const Default : Double ) : Double; overload;
  procedure SetConfig( const Section, Ident : String; const Value : Boolean ); overload;
  function  GetConfig( const Section, Ident : String; const Default : Boolean ) : Boolean; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TDateTime ); overload;
  function  GetConfig( const Section, Ident : String; const Default : TDateTime ) : TDateTime; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TDate ); overload;
  function  GetConfig( const Section, Ident : String; const Default : TDate ) : TDate; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TTime ); overload;
  function  GetConfig( const Section, Ident : String; const Default : TTime ) : TTime; overload;
  procedure SetConfig( const Section, Ident : String; const Value : TStream ); overload;
  function  GetConfig( const Section, Ident : String; Value : TStream ) : Integer; overload;

{------------------------------------------------------------------------------}
{ ExtractFileOrDirectory allows the extraction of the Nth file or directory in }
{ the Path string such that:  Loc = 0, returns the left most file/directory    }
{ name, Loc = 1, returns the second left-most, ... Loc = -1, returns the right }
{ most file/directory name, Loc = -2, returns the second right-most file/      }
{ directory name.                                                              }
{------------------------------------------------------------------------------}
  function ExtractFileOrDirectory( Loc : Integer; Path : String ) : String;

{==============================================================================}
{ TTreeview Support stuff.                                                     }
{==============================================================================}

  type
    {: Callback to use to copy the data of a treenode when the
       node itself is copied by CopySubtree.
     @param oldnode is the old node
     @param newnode is the new node
     @Desc Use a callback of this type to implement your own algorithm
       for a node copy. The default just uses the Assign method, which
       produces a shallow copy of the nodes Data property. }
    TCopyDataProc = procedure(oldnode, newnode : TTreenode);
    TCopyDataProcObject = procedure(oldnode, newnode : TTreenode) of object;

  {-- CopySubtree
  -------------------------------------------------------}
  {: Copies the source node with all child nodes to the target treeview.
  @Param sourcenode is the node to copy
  @Param target is the treeview to insert the copied nodes into
  @Param targetnode is the node to insert the copy under, can be nil to
    make the copy a top-level node.
  @Param CopyProc is the (optional) callback to use to copy a node.
    If Nil is passed for this parameter theDefaultCopyDataProc will be
  used.
  @Precondition  sourcenode <> nil, target <> nil, targetnode is either
    nil or a node of target
  @Raises Exception if targetnode happens to be in the subtree rooted in
    sourcenode. Handling that special case is rather complicated, so we
    simply refuse to do it at the moment.
  }{ Created 2003-04-09 by P. Below -----------------------------------------------------------------------
  }
  {function CopySubtree(sourcenode : TTreenode; target : TTreeview;
    targetnode : TTreenode; CopyProc : TCopyDataProc = nil) : TTreeNode; overload;}

  { TODO 3 -odonz -cTest : Implement Unit Test }
  function CopySubtree( sourcenode : TTreenode; target : TTreeview;
                        targetnode : TTreenode; CopyProc : TCopyDataProcObject;
                        LabelCopy : Boolean; IsChild : Boolean = False) : TTreeNode; overload;

  procedure CopyTreeView( SourceTree, DestTree : TTreeview; { TODO 3 -odonz -cTest : Implement Unit Test }
                          CopyProc : TCopyDataProcObject;
                          LabelCopy : Boolean = False);

{==============================================================================}
{ functions to change between windows and HTML color order                     }
{==============================================================================}

function ColorSwap( Value : Integer ) : Integer;
function CSSColor( Value : Integer ) : String; // Borland and HTML order differs

{==============================================================================}
{ Debug                                                                        }
{==============================================================================}
procedure MessageBox( What : String );
procedure Debug( const Message : String ); overload;
procedure Debug( const Value : Integer ); overload;
procedure Debug( const Value : Double ); overload;
procedure DebugLn( const Message : String = '');
procedure Stub( ForWhat : String );

{==============================================================================}
{ Misc.                                                                        }
{==============================================================================}
function DefaultSaveLocation: string;
function ExePath : String;
function ExeName : String;

{------------------------------------------------------------------------------}
{ Function type definitions for UnitTest driver program                        }
{------------------------------------------------------------------------------}

type
  TColorSwapTest              = function( Value  : Integer )  : Integer of object;
  TCopyFileTest               = function( F0, F1 : String )  : Boolean of object;
  TCSSColorTest               = function( Value  : Integer ) : String of object;
  TExeNameTest                = function : String of object;
  TExtractFileOrDirectoryTest = function( Loc : Integer; Value : String ) : String of object;
  TMinMaxFloatTest            = function( V0, V1 : Extended ) : Extended of object;
  TMinMaxIntegerTest          = function( V0, V1 : Integer ) : Integer of object;
  TReadBoolTest               = function( Src : String; Line : Integer ) : Boolean of object;
  TRectToOriginTest           = function( Value : TRect ) : TRect of object;
  TStringToStringTest         = function( Value : String ) : String of object;
  TSetPositionCBTest          = function( CB : TComboBox; TValue : String ) : Integer of object;
  TSetPositionRGTest          = function( CB : TRadioGroup; TValue : String ) : Integer of object;
  TWalkDirectoryTreeTest      = function( SubDirsFirst : Boolean ) : Boolean of object;
  TCopyFilesTest              = function( Src, Dst : String ) : Boolean of object;

implementation

uses
  Dialogs, Forms, LCLProc, FileUtil, IniFiles
  {$ifdef WIN32}
  , Windows
  {$endif};

procedure ReadBool( var F : TextFile; var Value : Boolean );
var
  S : String;
begin
  Readln(F,S);
  Value := S = 'TRUE';
end;

function StringToFloat(Value: String): Extended;
begin
  if Value = '' then
    Result := 0
  else
    Result := StrToFloat( Value );
end;

function StringToInt(Value: String): Integer;
begin
  if Value = '' then
    Result := 0
  else
    Result := StrToInt( Value );
end;

function RectToOrigin(Rect: TRect): TRect;
var
  R : TRect;
begin
  R.Left := 0;
  R.Top  := 0;
  R.Right := Rect.Right - Rect.Left;
  R.Bottom := Rect.Bottom - Rect.Top;
  Result := R;
end;

function Equal(R0, R1 : TRect ): Boolean;
begin
  Result := (R0.Left    = R1.Left   ) and
            (R0.Top     = R1.Top    ) and
            (R0.Right   = R1.Right  ) and
            (R0.Bottom  = R1.Bottom );
end;

procedure AngleTextOut(ACanvas: TCanvas; Angle, X, Y: Integer; Str: string);
{$ifdef WIN32}
var
  LogRec: TLogFont;
  OldFontHandle,
  NewFontHandle: hFont;
begin
  GetObject(ACanvas.Font.Handle, SizeOf(LogRec), Addr(LogRec));
  LogRec.lfEscapement := Angle*10;
  NewFontHandle := CreateFontIndirect(LogRec);
  OldFontHandle := SelectObject(ACanvas.Handle, NewFontHandle);
  ACanvas.TextOut(X, Y, Str);
  NewFontHandle := SelectObject(ACanvas.Handle, OldFontHandle);
  DeleteObject(NewFontHandle);
{$else WIN32}
begin
  Stub('Linux version of AngleTextOut');
{$endif}
end;

function Min(V0, V1: Double): Double;
begin
  if V0 < V1 then
    Result := V0
  else
    Result := V1;
end;

function Min(V0, V1: Integer): Integer;
begin
  if V0 < V1 then
    Result := V0
  else
    Result := V1;
end;

function Max(V0, V1: Double): Double;
begin
  if V0 > V1 then
    Result := V0
  else
    Result := V1;
end;

function Max(V0, V1: Integer): Integer;
begin
  if V0 > V1 then
    Result := V0
  else
    Result := V1;
end;

function AreYouSure( Prompt : String; withCancel : Boolean ) : Integer;
begin
  if withCancel then
    Result := MessageDlg( Prompt + #13#10'Are You Sure?',
                          mtConfirmation, [mbYes, mbNO, mbCancel ], 0)
  else
    Result := MessageDlg( Prompt + #13#10'Are You Sure?',
                          mtConfirmation, [mbYes, mbNO ], 0);
end;

procedure SetPositionRG(RG: TRadioGroup; Value: String);
var
  I : Integer;
begin
  RG.ItemIndex := -1; // Default if no match
  for I := 0 to pred(RG.Items.Count) do
    begin
      if RG.Items[I] = Value then
        begin
          RG.ItemIndex := I;
          exit;
        end;
    end;
end;

procedure SetPositionCB(CB: TComboBox; Value: String);
var
  I : Integer;
begin
  CB.ItemIndex := -1; // Default if no match
  for I := 0 to pred(CB.Items.Count) do
    begin
      if CB.Items[I] = Value then
        begin
          CB.ItemIndex := I;
          exit;
        end;
    end;
end;

{==============================================================================}
{ TTreeview Support stuff.                                                     }
{==============================================================================}

{: The default operation is to do a shallow copy of the node, via
Assign. }
procedure DefaultCopyDataProc(oldnode, newnode : TTreenode);
begin
  newnode.Assign(oldnode);
end;

{-- CopySubtree
-------------------------------------------------------}
{: Copies the source node with all child nodes to the target treeview.
@Param sourcenode is the node to copy
@Param target is the treeview to insert the copied nodes into
@Param targetnode is the node to insert the copy under, can be nil to
  make the copy a top-level node.
@Param CopyProc is the (optional) callback to use to copy a node.
  If Nil is passed for this parameter theDefaultCopyDataProc will be
used.
@Precondition  sourcenode <> nil, target <> nil, targetnode is either
  nil or a node of target
@Raises Exception if targetnode happens to be in the subtree rooted in
  sourcenode. Handling that special case is rather complicated, so we
  simply refuse to do it at the moment.
}{ Created 2003-04-09 by P. Below -----------------------------------------------------------------------
}
{$ifdef NO}
function CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProc = nil) : TTreeNode;
var
  anchor : TTreenode;
  child : TTreenode;
begin { CopySubtree }
  Assert(Assigned(sourcenode),
    'CopySubtree:sourcenode cannot be nil');
  Assert(Assigned(target),
    'CopySubtree: target treeview cannot be nil');
  Assert((targetnode = nil) or (targetnode.TreeView = target),
    'CopySubtree: targetnode has to be a node in the target treeview.');

  if (sourcenode.TreeView = target) and
    (targetnode.HasAsParent(sourcenode) or (sourcenode =
    targetnode)) then
    raise Exception.Create('CopySubtree cannot copy a subtree to one of the ' +
      'subtrees nodes.');

  if not Assigned(CopyProc) then
    CopyProc := DefaultCopyDataProc;

  anchor := target.Items.AddChild(targetnode, sourcenode.Text);
  CopyProc(sourcenode, anchor);
  anchor.Text := anchor.Text  + ' [Copy]';
  Result := anchor;
  child := sourcenode.GetFirstChild;
  while Assigned(child) do
  begin
    CopySubtree(child, target, anchor, CopyProc);
    child := child.getNextSibling;
  end; { While }
end; { CopySubtree }
{$endif}
function CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProcObject;
  LabelCopy : Boolean; IsChild : Boolean) : TTreeNode;
var
  anchor : TTreenode;
  child : TTreenode;
begin { CopySubtree }
  Assert(Assigned(sourcenode),
    'CopySubtree:sourcenode cannot be nil');
  Assert(Assigned(target),
    'CopySubtree: target treeview cannot be nil');
  Assert((targetnode = nil) or (targetnode.TreeView = target),
    'CopySubtree: targetnode has to be a node in the target treeview.');

  if (sourcenode.TreeView = target) and ((targetnode <> nil) and
    (targetnode.HasAsParent(sourcenode) or (sourcenode =
    targetnode))) then
    raise Exception.Create('CopySubtree cannot copy a subtree to one of the ' +
      'subtrees nodes.');

  anchor := target.Items.AddChild(targetnode, sourcenode.Text);
  CopyProc(sourcenode, anchor);
  if IsChild and LabelCopy then
    anchor.Text := anchor.Text  + ' [Copy]';
  Result := anchor;
  child := sourcenode.GetFirstChild;
  while Assigned(child) do
  begin
    CopySubtree(child, target, anchor, CopyProc, true);
    child := child.getNextSibling;
  end; { While }
end; { CopySubtree }

procedure CopyTreeView( SourceTree, DestTree : TTreeview;
                        CopyProc : TCopyDataProcObject;
                        LabelCopy : Boolean);
var
  Node : TTreeNode;
begin
  DestTree.Items.Clear;
  if SourceTree.Items.Count = 0 then exit;
  Node := SourceTree.Items[0];
  while Node <> nil do
    begin
      CopySubtree( Node, DestTree,nil,CopyProc,LabelCopy);
      Node := Node.GetNextSibling;
    end;
end;

function ColorSwap(Value: Integer): Integer;
var
  R, G, B : Word;
begin
{ Windows :
    Red   = $0000ff
    Green = $00ff00
    Blue  = $ff0000

  HTML :
    Red   = $ff0000
    Green = $00ff00
    Blue  = $0000ff
}
  B := Value and $ff;
  G := Value and $ff00 shr 8;
  R := Value and $ff0000 shr 16;

  Result := R or (G shl 8) or (B shl 16);
end;

function CSSColor(Value: Integer): String;
begin
  Result := IntToHex( Value, 6);
end;

{==============================================================================}
{ Debug                                                                        }
{==============================================================================}
procedure MessageBox(What: String);
begin
  MessageDlg(What,mtInformation,[mbOk],0);
end;

procedure Debug(const Message: String);
begin
{$ifdef WIN32}
  OutputDebugString( PChar(Message) );
{$else}
 DbgOut( Message );
{$endif}
end;

procedure Debug(const Value: Integer);
begin
  DbgOut( IntToStr( Value ) );
end;

procedure Debug(const Value: Double);
begin
  DbgOut( FloatToStr( Value ) );
end;

procedure DebugLn(const Message: String);
begin
  LCLProc.DebugLn( Message );
end;

procedure Stub(ForWhat: String);
begin
  MessageBox( ForWhat + ' not implemented.' );
end;

function DefaultSaveLocation: string;
begin
 // Result := GetAppConfigDir( False );
  Result := GetUserDir;
  {$ifdef WIN32}
//  Result := Result + DirectorySeparator;
  {$endif}
  Result := Result + ApplicationName + DirectorySeparator;
  if not DirectoryExists( Result ) then ForceDirectories( Result );
end;

function ExePath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function ExeName: String;
var
  P : Integer;
begin
  Result := ExtractFileName(ParamStr(0));
  P := Pos('.',Result);
  if P > 0 then
    Result := Copy(Result,1,P-1);
end;

procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                             Attributes              : Integer;
                             SubDirsFirst            : Boolean;
                             DoSomething             : TProcessFile;
                             UserData                : TObject;
                             Stop                    : PBoolean );
var
  Depth : Integer;
begin
  Depth := 0;
  WalkDirectoryTree1( BasePath, RelPath, Mask, Attributes, SubDirsFirst,
                      DoSomething, UserData, depth, Stop );
end;

procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                              Attributes              : Integer;
                              SubDirsFirst            : Boolean;
                              DoSomething             : TProcessFile;
                              UserData                : TObject;
                              Depth                   : Integer;
                              Stop                    : PBoolean );
var
  SR: TSearchRec;
  SearchPath : String;
  NewRelPath : String; // For debug and test
  X :Integer;
  function Go : Boolean;
  begin
    if Assigned(Stop) then
      begin
        Application.ProcessMessages;
        Result := not  Stop^;
      end
    else
      Result := True;
  end;
  procedure NormalFiles( SearchPath : String; Attributes : Integer; SR : TSearchRec );
  begin
    if (FindFirst( SearchPath, Attributes - faDirectory, SR) = 0) and Go then
      begin
        DoSomething(BasePath,RelPath,SR,Depth,UserData);

        while (FindNext( SR ) = 0) and Go do
          DoSomething( BasePath,RelPath,SR,Depth,UserData);
      end;
    Sysutils.FindClose( SR );
  end;
  procedure Directories( SearchPath, RelPath : String; SR : TSearchRec );
  begin
    if (SR.Attr and faDirectory) = faDirectory then
      if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          NewRelPath := RelPath + SR.Name + DirectorySeparator;
          if SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
          WalkDirectoryTree1( BasePath,NewRelPath,
                             Mask,
                             Attributes,
                             SubDirsFirst,
                             DoSomething,
                             UserData,
                             Depth+1,
                             Stop );
          if not SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
        end;
  end;

begin
  if BasePath[Length(BasePath)] <> DirectorySeparator then
    BasePath := BasePath + DirectorySeparator;
  SearchPath := BasePath + RelPath + Mask;

// Handle the normal files first if requested
  if not SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
// Now handle the directories
  if (FindFirst( SearchPath, faDirectory, SR ) = 0) and Go then
    begin
      Directories( SearchPath, RelPath, SR );
      while (FindNext(SR) = 0) and go do
        begin
          Directories( SearchPath, RelPath, SR );
        end;
    end;

  Sysutils.FindClose( SR );
  // Handle the normal files if requested
  if SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
end;

procedure WalkDirectoryTree( BasePath, RelPath, Mask : String;
                             Attributes              : Integer;
                             SubDirsFirst            : Boolean;
                             DoSomething             : TProcessFileObject;
                             UserData                : TObject;
                             Stop                    : PBoolean );
var
  Depth : Integer;
begin
  Depth := 0;
  WalkDirectoryTree1( BasePath, RelPath, Mask, Attributes, SubDirsFirst,
                      DoSomething, UserData, depth, Stop );
end;

procedure WalkDirectoryTree1( BasePath, RelPath, Mask : String;
                              Attributes              : Integer;
                              SubDirsFirst            : Boolean;
                              DoSomething             : TProcessFileObject;
                              UserData                : TObject;
                              Depth                   : Integer;
                              Stop                    : PBoolean );
var
  SR: TSearchRec;
  SearchPath : String;
  NewRelPath : String; // For debug and test
  function Go : Boolean;
  begin
    if Assigned(Stop) then
      begin
        Application.ProcessMessages;
        Result := not  Stop^;
      end
    else
      Result := True;
  end;
  procedure NormalFiles( SearchPath : String; Attributes : Integer; SR : TSearchRec );
  begin
    if (FindFirst( SearchPath, Attributes - faDirectory, SR) = 0) and Go then
      begin
        DoSomething(BasePath,RelPath,SR,Depth,UserData);

        while (FindNext( SR ) = 0) and Go do
          DoSomething( BasePath,RelPath,SR,Depth,UserData);
      end;
    Sysutils.FindClose( SR );
  end;
  procedure Directories( SearchPath, RelPath : String; SR : TSearchRec );
  begin
    if (SR.Attr and faDirectory) = faDirectory then
      if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          NewRelPath := RelPath + SR.Name + DirectorySeparator;
          if SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
          WalkDirectoryTree1( BasePath,NewRelPath,
                             Mask,
                             Attributes,
                             SubDirsFirst,
                             DoSomething,
                             UserData,
                             Depth+1,
                             Stop );
          if not SubDirsFirst then
            DoSomething(BasePath,RelPath,SR,Depth,UserData);
        end;
  end;

begin
  if BasePath[Length(BasePath)] <> DirectorySeparator then
    BasePath := BasePath + DirectorySeparator;
  SearchPath := BasePath + RelPath + Mask;

// Handle the normal files first if requested
  if not SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
// Now handle the directories
  if (FindFirst( SearchPath, faDirectory, SR ) = 0) and Go then
    begin
      Directories( SearchPath, RelPath, SR );
      while (FindNext( SR ) = 0) and Go do
        Directories( SearchPath, RelPath, SR );
    end;
  Sysutils.FindClose( SR );
  if SubDirsFirst then
    NormalFiles( SearchPath, Attributes, SR);
end;

procedure RmFiles(Base, Rel: String; const SR: TSearchRec; const Depth: Integer
  );
var
  Path : String;
begin
  Path := Base + Rel + DirectorySeparator + SR.Name;
  Debug('Deleting:  [' + Path + ']');
  if SR.Attr = faDirectory then
    begin
     if (Depth > 0) and not ((SR.Name = '.') or (SR.Name = '..')) then
       RmDir( Path)
    end
  else
    SysUtils.DeleteFile( Path );
end;

procedure CopyFile(Src, Dst : String);
var
  PathToDirectory : String;
  S : String;
  P : PChar;
begin
  PathToDirectory := ExtractFilePath(Dst);
  if not DirectoryExists(PathToDirectory) then
    ForceDirectories(PathToDirectory);
  if Src <> Dst then
{$ifdef WIN32}
    if not Windows.CopyFile(Pchar(Src),Pchar(Dst),False) then
      begin
        FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER,
                       nil,
                       GetLastError,0,
                       @P,
                       0,nil);
                       S := P;
        raise Exception.Create('Failed to copy file from ' + Src +' to '+ Dst + #13#10+S);
      end;
{$else}
    FileUtil.Copyfile( Src, Dst );
{$endif WIN32}
end; { CopyFile(Src, Dst : String); }

type TCopyFilesData = class(TObject)
  FromPath : String;
  ToPath   : String;
  constructor Create( aFromPath, aToPath : String);
end;

constructor TCopyFilesData.Create( aFromPath, aToPath : String );
begin
  FromPath := aFromPath;
  ToPath   := aToPath;
end;

procedure CP(       BasePath, RelPath : String;
              const SR                : TSearchRec;
              const Depth             : Integer;
                    UserData          : TObject );
var
  CPData : TCopyFilesData;
  Kind   : String;
  Src, Dst : String;
begin
  CPData := UserData as TCopyFilesData;
  Src := CPData.FromPath + DirectorySeparator + RelPath;
  Dst := CPData.ToPath + DirectorySeparator + RelPath;
  if (SR.Attr and faDirectory) = faDirectory then
    begin
      ForceDirectories( Dst + DirectorySeparator + SR.Name );
      Kind := 'Dir ';
    end
  else
    begin
      Kind := 'File';
      Src := Src + SR.Name;
      Dst := Dst + SR.Name;
      CopyFile( Src, Dst );
    end;
end;

function CopyFiles( FromPath, ToPath, Wildcard : String; Force : Boolean = false ) : Boolean;
var
  UserData : TCopyFilesData;
begin
  UserData := TCopyFilesData.Create( FromPath, ToPath );

  WalkDirectoryTree( FromPath, '', '*', faAnyFile, True, @CP, UserData);
end;  { CopyFiles( FromPath, ToPath, Wildcard : String; ... }

function RemoveExt( FilePath : String ) : String;
var
  Ext : String;
  P   : Integer;
begin
  Ext := ExtractFileExt( FilePath );
  P := Pos(Ext,FilePath);
  if P = 0 then
    Result := FilePath
  else
    Result := Copy(FilePath,1,P-1);
end;

function BuildDateTime: String;
var
  FAge : Integer;
  fDate : TDateTime;
begin
  fAge := FileAge(ParamStr(0));
  fDate := FileDateToDateTime( fAge );
  Result := FormatDateTime('dddddd tt',fDate);
end;

function ExtractFileOrDirectory(Loc: Integer; Path: String): String;
  function RPos( Sep : String; Str : String ) : Integer;
  var
    I : Integer;
  begin
    for I := Length(Str) downto 1 do
      if Str[I] = DirectorySeparator then
        begin
          Result := I;
          exit;
        end;
    Result := 0;
  end;
var
  Temp : String;
  I    : Integer;
  S    : Integer;
begin
  Temp := Path;
  Result := '';
  if Loc >= 0 then
    begin
      for I := 0 to Loc do
        begin
          S := Pos( DirectorySeparator, Temp );
          if S = 1 then
            Temp := Copy(Temp,2,MaxPathLen);
          S := Pos( DirectorySeparator, Temp );
          Result := Copy(Temp,1,S-1);
          Temp := Copy( Temp, S+1,MaxPathLen );
        end;
    end
  else
    begin
      for I := 1 to -Loc do
        begin
          S := RPos( DirectorySeparator, Temp );
          if S = Length(Temp) then
            Temp := Copy(Temp,1,S-1);
          S := RPos( DirectorySeparator, Temp );
          Result := Copy(Temp,S+1,MaxPathLen);
          Temp := Copy(Temp,1,S-1);
        end;
    end;
end;

function CountFilesOrDirectories( Path : String ) : Integer;
var
  I : Integer;
begin
  Result := 0;
  if Path[1] <> DirectorySeparator then
    Inc(Result);
  if Path[Length(path)] = DirectorySeparator then
    Dec(Result);
  for I := 1 to Length(Path) do
    if Path[I] = DirectorySeparator then
      Inc(Result);
end;

function OpenIniFile : TIniFile; // Local to this unit
var
  IniFileName : String;
  IniFilePath : String;
begin
  IniFileName := GetAppConfigFile( False );
  IniFilePath := ExtractFilePath( IniFileName );
  if not FileExists( IniFilePath ) then
    ForceDirectories( IniFilePath );
  Result := TIniFile.Create( IniFileName );
end;

procedure SetConfig(const Section, Ident: String; const Value: String);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteString( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: String): String;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadString( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: Integer);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteInteger( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: Integer
  ): Integer;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadInteger( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: Double);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteFloat( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: Double): Double;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadFloat( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: Boolean);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteBool( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: Boolean
  ): Boolean;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadBool( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TDateTime);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteDateTime( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: TDateTime
  ): TDateTime;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadDateTime( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TDate);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteDate( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: TDate): TDate;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadDate( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TTime);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteTime( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; const Default: TTime): TTime;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadTime( Section, Ident, Default );
  finally
    IniFile.Free;
  end;
end;

procedure SetConfig(const Section, Ident: String; const Value: TStream);
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    IniFile.WriteBinaryStream( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

function GetConfig(const Section, Ident: String; Value : TStream
  ): Integer;
var
  IniFile : TIniFile;
begin
  IniFile := OpenIniFile;
  try
    Result := IniFile.ReadBinaryStream( Section, Ident, Value );
  finally
    IniFile.Free;
  end;
end;

end.

