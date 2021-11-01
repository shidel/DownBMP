{
    BSD 3-Clause License
    Copyright (c) 2021, Jerome Shidel
    All rights reserved.
}

program DownBMP;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Classes, SysUtils, CustApp, IniFiles, StrUtils, Graphics, LCLType,
     IntfGraphics, fpImage, AvgLvlTree, DangerPAL, WinBMP3x;

{$I version.inc}
const
  CfgExt = '.CFG';
  VerboseOpts : array of string = ('Quite', 'Normal', 'Verbose');

type

  { TColorData }

  TColorData = class
  public
    Count : Int64;
    Color : TFPColor;
    Replace : Pointer;
    Idx : Int64;
    constructor Create(aColor : TFPColor; aCount : Int64);
    function GetNewColor : TFPColor;
    function GetNewIndex : integer;
  end;

function CompareColorValues(Data1, Data2 : pointer) : integer;
var
   V1, V2 : DWord;
begin
  with TColorData(Data1).Color do
    V1 := Red shl 16 + Green shl 8 + Blue;
  with TColorData(Data2).Color do
    V2 := Red shl 16 + Green shl 8 + Blue;
  if V1 < V2 then
    Result := -1
  else if V1 > V2 then
    Result := 1
  else
    Result := 0;
end;

function CompareColorCount(Data1, Data2 : pointer) : integer;
begin
  if TColorData(Data1).Count < TColorData(Data2).Count then
    Result := -1
  else if TColorData(Data1).Count > TColorData(Data2).Count then
    Result := 1
  else
    Result := 0;
end;

function CompareColorIndex(Data1, Data2 : pointer) : integer;
begin
  if TColorData(Data1).Idx < TColorData(Data2).Idx then
    Result := -1
  else if TColorData(Data1).Idx > TColorData(Data2).Idx then
    Result := 1
  else
    Result := 0;
end;

type

  { TDownShiftBMP }

  TDownShiftBMP = class(TCustomApplication)
  private
  protected
      CfgFile  : TIniFile;
      Verbose  : integer;
      PreserveAspect,
      BestMatch : boolean;
      MaxWidth : integer;
      MaxHeight : integer;
      MaxColors : integer;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure SettingsDefault; virtual;
    procedure SettingsLoad; virtual;
    procedure SettingsSave; virtual;
    procedure ProcessMain; virtual;
    procedure ReduceColors(var Colors : TAvgLvlTree);
    procedure AddDangerPAL(var Colors : TAvgLvlTree; Offset : Int64);
    procedure IndexColors(var Colors : TAvgLvlTree; var CP : TWinBMPPalette);
  end;

{ TColorData }

constructor TColorData.Create(aColor: TFPColor; aCount: Int64);
begin
  inherited Create;
  Count := aCount;
  Color := AColor;
  Replace := nil;
  Idx := -1;
end;

function TColorData.GetNewColor: TFPColor;
begin
  if Assigned(Replace) then
    Result := TColorData(Replace).GetNewColor
  else
     Result := Color;
end;

function TColorData.GetNewIndex : integer;
begin
  if Assigned(Replace) then
    Result := TColorData(Replace).GetNewIndex
  else
     Result := Idx;
end;

{ TDownShiftBMP }

procedure TDownShiftBMP.DoRun;
var
  ErrorMsg: String;
  TW, TH, I, E, P : integer;
  S : String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hsamc', ['help', 'size', 'aspect', 'match', 'colors']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('a', 'aspect') then begin
     PreserveAspect := True;
  end;

  if HasOption('m', 'match') then begin
     BestMatch := True;
  end;

  if HasOption('s', 'size') then begin
    MaxWidth:=64;
    MaxHeight:=48;
    if GetOptionValue('s', 'size') <> '' then begin
      MaxWidth := -1;
      MaxHeight := -1;
      S := GetOptionValue('s', 'size');
      P := Pos(',', S);
      if P < 1 then P := Pos(':', S);
      if P < 1 then P := Pos(',', S);
      if P < 1 then P := Pos('X', S);
      if P < 1 then P := Pos('x', S);
      if P < 1 then P := Length(S) + 1;
      Val(Copy(S, 1, P - 1), I, E);
      if E = 0 then
         MaxWidth := I
      else
         MaxWidth := -1;
      Val(Copy(S, P + 1, Length(S)), I, E);
      if E = 0 then
         MaxHeight := I
      else
         MaxHeight := -1;
    end;
  end;

   if HasOption('c', 'colors') then begin
    MaxColors:=256;
    if GetOptionValue('c', 'colors') <> '' then begin
      S := GetOptionValue('c', 'colors');
      Val(S, I, E);
      if (E = 0) and (I >1) and (I<256) then
         MaxColors := I
      else
         MaxWidth := 256;
    end;
  end;


  ProcessMain;

  // stop program loop
  Terminate;
end;

constructor TDownShiftBMP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  CfgFile := TIniFile.Create(LowerCase(ChangeFileExt(ExeName, CfgExt)));
  MaxWidth := -1;
  MaxHeight := -1;
  MaxColors := 256;
  PreserveAspect := False;
  BestMatch := False;
  SettingsDefault;
  SettingsLoad;
end;

destructor TDownShiftBMP.Destroy;
begin
  { SettingsSave; }
  CfgFile.Free;
  inherited Destroy;
end;

procedure TDownShiftBMP.WriteHelp;
begin
  WriteLn(APP_PRODUCTNAME, ' version ', APP_VERSION);
  WriteLn('converts images to uncompressed Windows 3.x 256 color bitmaps');
  WriteLn;
  writeLn('Usage: ', ExtractFileName(ExeName), ' [options] [filename...]');
  WriteLn;
  WriteLn(#9,'-h, --help',    #9#9, 'Display this help text');
  WriteLn;
  WriteLn(#9,'-c, --colors n',    #9#9, 'Reduce to less then 256 colors (-m highly recommended)');
  WriteLn(#9,'-s, --size x:y',    #9#9, 'Resize image (hint: leave out value for autosize)');
  WriteLn(#9,'-m, --match',       #9#9, 'Match Danger Engine Color default profile');
  WriteLn;

  WriteLn(#9,'-v, --verbose', #9#9, 'Display more information');
{  WriteLn(#9,'-q, --quite',   #9#9, 'Display less information'); }
  WriteLn;

end;

procedure TDownShiftBMP.SettingsDefault;
begin
end;

procedure TDownShiftBMP.SettingsLoad;
begin
  Verbose := IndexText(Trim(CfgFile.ReadString('*', 'Verbose', '')), VerboseOpts) - 1;
  if Verbose < -1 then Verbose := 0;
end;

procedure TDownShiftBMP.SettingsSave;
begin
  CfgFile.WriteString('*', 'Verbose', VerboseOpts[Verbose + 1]);
end;

procedure ResizePicture(var P: TPicture; NewWidth, NewHeight: word; Aspect : boolean);
var
  a: TBitmap;
  b: TBitmap;
begin
  try
    a := TBitmap.Create;
    b := TBitmap.Create;
    a.SetSize(P.Width, P.Height);
    a.Assign(P.Bitmap);
    b.SetSize(NewWidth, NewHeight);
    { b.Canvas.FillRect(b.Canvas.ClipRect); }
    b.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), a);
    P.Clear;
    P.Assign(b);
{    P.Width := NewWidth;
    P.Height := NewHeight;
    P.Canvas.Clear;
    P.Canvas.Draw(0, 0, b); }
  finally
    FreeAndNil(b);
    FreeAndNil(a);
  end;
end;

function GrayScale(C : TFPColor) : word;
begin
  Result := (DWord(C.Red) * 299 + DWord(C.Green) * 587 + DWord(C.Blue) * 114) div 1000;
end;

procedure ColorWeight(var Color : TFPColor);
begin
  with Color do begin
    Red := Red * 100;
    Green := Green * 195;
    Blue := Blue * 38;
  end;
end;

function ColorDistance(C1, C2 : TFPColor) : DWord;
begin
  ColorWeight(C1);
  ColorWeight(C2);
  Result := Trunc(sqrt(sqr(C1.Red - C2.Red) + sqr(C1.Green - C2.Green) + sqr(C1.Blue - C2.Blue)));
end;

procedure DownColor(var C : word);
var
  CC : DWord;
begin
   CC := C;
   (* Don't need to adjust for max color value *)
   { CC := CC * $ffff;
   CC := CC div $fcfc; }
   CC := CC shr 8;
   if CC > $ff then CC := $ff;
   C := CC;
end;

procedure TDownShiftBMP.ProcessMain;
var
  Colors, NewColors : TAvgLvlTree;
  Search : TColorData;
  Node: TAvgLvlTreeNode;
  TC : TColorData;
  B : TPicture;
  F : TLazIntfImage;
  I : integer;
  X, Y, LW : word;
  C : TFPColor;
  O : TBytes;
  CP : TWinBMPPalette;
begin
  for I := 1 to ParamCount do begin
    if Copy(ParamStr(I), 1,1) = '-' then continue;
    if Not FileExists(ParamStr(I)) then continue;
    B := TPicture.Create;
    F := TLazIntfImage.Create(0,0);
    C.Red := 0;
    C.Green:= 0;
    C.Blue := 0;
    Colors := TAvgLvlTree.Create(@CompareColorValues);
    NewColors := TAvgLvlTree.Create(@CompareColorValues);
    Search := TColorData.Create(C,0);
    try
       Write(ParamStr(I), ':');
       B.LoadFromFile(ParamStr(I));
       Write(' ', B.Width, 'x', B.Height);
       { resize the image if needed }
       if (MaxWidth <> -1) or (MaxHeight <> -1) then begin
          if (MaxWidth = -1) then begin
             MaxWidth := B.Width * MaxHeight div B.Height;
          end;
          if (MaxHeight = -1) then begin
             MaxHeight := B.Height * MaxWidth div B.Width;
          end;
          ResizePicture(B, MaxWidth, MaxHeight, PreserveAspect);
          Write(' => ', MaxWidth, 'x', MaxHeight);
       end;
       F.LoadFromBitmap(B.Bitmap.Handle, B.Bitmap.MaskHandle);
       { get current color counts }
       for Y := 0 to F.Height - 1 do begin
           for X := 0 to F.Width - 1 do begin
             C := F.Colors[x,y];
             DownColor(C.Red);
             DownColor(C.Green);
             DownColor(C.Blue);
             Search.Color := C;
             Node := Colors.Find(Search);
             if not Assigned(Node) then
                Colors.Add(TColorData.Create(C,1))
             else begin
                Inc(TColorData(Node.Data).Count);
             end;
           end;
       end;
       Write(', ', Colors.Count, ' colors');
       { if best match add Danger Engine Pallete as most needed }
       ReduceColors(Colors);
       if BestMatch then begin
          AddDangerPAL(Colors, MaxWidth * MaxHeight);
          ReduceColors(Colors);
       end;
       { get new color counts }
       for Y := 0 to F.Height - 1 do begin
           for X := 0 to F.Width - 1 do begin
               C := F.Colors[x,y];
               DownColor(C.Red);
               DownColor(C.Green);
               DownColor(C.Blue);
               Search.Color := C;
               Node := Colors.Find(Search);
               if not Assigned(Node) then Halt(99);
               C := TColorData(Node.Data).GetNewColor;
               Search.Color := C;
               Node := NewColors.Find(Search);
               if not Assigned(Node) then
                  NewColors.Add(TColorData.Create(C,1))
               else begin
                  Inc(TColorData(Node.Data).Count);
               end;
           end;
       end;
       IndexColors(NewColors, CP);
       Write(', ', NewColors.Count, ' colors ');
       { generate new bitmap }
       LW := F.Width and $fffc;
       if LW <> F.Width then Inc(LW, 4);
       SetLength(O, F.Height * LW);
       for X := 0 to Length(O) do O[X] := 0;
       if (F.Height <= 48) and (F.Width <= 80) then WriteLn;
       for Y := 0 to F.Height - 1 do begin
           for X := 0 to F.Width - 1 do begin
             C := F.Colors[x,y];
             DownColor(C.Red);
             DownColor(C.Green);
             DownColor(C.Blue);
             Search.Color := C;
             Node := Colors.Find(Search);
             if not Assigned(Node) then Halt(99);
             C := TColorData(Node.Data).GetNewColor;
             Search.Color := C;
             Node := NewColors.Find(Search);
             if not Assigned(Node) then Halt(69);
             C := TColorData(Node.Data).GetNewColor;
             O[Y * LW + X] := TColorData(Node.Data).GetNewIndex;
             if (F.Height <= 48) and (F.Width <= 80) then
                case GrayScale(C) of
                    0 : Write(' ');
                    1..49 : Write('.');
                    50..99 : Write('-');
                    100..149 : Write('+');
                    150..199 : Write('*');
                    200..255 : Write('#');
                 end;
           end;
           if (F.Height <= 48) and (F.Width <= 80) then WriteLn;
       end;
       WriteLn('=> ', SaveWinBMP3x(ParamStr(I), F.Width, F.Height, O, CP, NewColors.Count));
    finally
       FreeAndNil(B);
       FreeAndNil(F);
       FreeAndNil(Colors);
       FreeAndNil(NewColors);
       FreeAndNil(Search);
    end;
  end;
end;

procedure TDownShiftBMP.ReduceColors(var Colors: TAvgLvlTree);
var
  BM, Node, Search: TAvgLvlTreeNode;
  BD, BC, TD, TC : Int64;
  CR : Int64;
{  SR : Int64; }
begin
  { WriteLn; }
  Colors.OnCompare := @CompareColorCount;
  CR := Colors.Count - MaxColors;
  {
  SR := 0;
  }
  for Node in Colors do begin
      if CR <= 0 then break;
      Dec(CR);
      {
      Inc(SR);
      }
      BD := System.MaxSIntValue;
      BM := nil;
      BC := 0;
      for Search in Colors do begin
          if Assigned(TColorData(Search.Data).Replace)
             or (Search = Node) then Continue;
          TD := ColorDistance(TColorData(Node.Data).Color,
             TColorData(Search.Data).Color);
          TC := TColorData(Search.Data).Count;
          if (TD < BD) or ((TD = BD) and (TC > BC)) then begin
             BD := TD;
             BC := TC;
             BM := Search;
          end;
      end;
      if not Assigned(BM) then Halt(86);
      {
      if (not BestMatch) or (SR < Colors.Count - 255) then begin
        with TColorData(Node.Data) do
          Write(Color.Red, ',', Color.Blue,',', Color.Green, ' ', Count, ' => ');
        with TColorData(BM.Data) do
          WriteLn(Color.Red, ',', Color.Blue,',', Color.Green, ' ', Count);
      end;
      }
      Inc(TColorData(BM.Data).Count, TColorData(Node.Data).Count);
      TColorData(Node.Data).Count := 0;
      TColorData(Node.Data).Replace := BM.Data;
  end;
  Colors.OnCompare := @CompareColorValues;
end;

procedure TDownShiftBMP.AddDangerPAL(var Colors: TAvgLvlTree; Offset: Int64);
var
  I : integer;
  C : TFPColor;
begin
   for I := 0 to 255 do begin
     C.Red := DANGER_RGB[I].Red;
     C.Green := DANGER_RGB[I].Green;
     C.Blue := DANGER_RGB[I].Blue;
     Colors.Add(TColorData.Create(C, Offset + 1024 - I));
   end;
end;

procedure TDownShiftBMP.IndexColors(var Colors: TAvgLvlTree;var CP : TWinBMPPalette);
var
   Node : TAvgLvlTreeNode;
   I : Integer;
begin
  Colors.OnCompare := @CompareColorCount;
  I := 0;
  for Node in Colors do begin
    TColorData(Node.Data).Idx:= I;
    CP[I].Red := TColorData(Node.Data).Color.Red;
    CP[I].Green := TColorData(Node.Data).Color.Green;
    CP[I].Blue := TColorData(Node.Data).Color.Blue;
    Inc(I);
  end;
  Colors.OnCompare := @CompareColorValues;
end;

var
  Application: TDownShiftBMP;

{$R *.res}

begin
  Application:=TDownShiftBMP.Create(nil);
  Application.Title:=APP_PRODUCTNAME;
  Application.Run;
  Application.Free;
end.

