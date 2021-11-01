unit WinBMP3x;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpImage;

{$PackRecords 1}
type
   TBytes = array of byte;
   tWinBMPColor = record
        Blue, Green, Red, Reserved : byte; { Backwards ;-( }
    end;

    TWinBMPPalette = array[0..255] of tWinBMPColor;

    function SaveWinBMP3x(FileName : String; Width, Height : word; Data : TBytes;
      Colors : TWinBMPPalette; ColorCount : word) : String;

implementation

type
    tWinBMPFileHeader = record { 14-bytes, Win 2.x + }
        FileType : Word;      { Should be 0x4d42 "BM" }
        FileSize : DWORD;     { DWORD }
        Reserved : DWORD;     { 2 Words, should be 0 }
        ImageOfs : DWORD;     { DWORD, start of image data }
    end;

    tWinBMPBitmapHeader = record { 40 bytes, Win 3.x + }
        HeadSize        : DWORD;  { DWORD, size of this header }
	    Width           : DWORD;  { Image Width }
	    Height          : DWORD;  { Image Height }
	    Planes          : word;     { Color Planes, should be 1 }
	    BitsPerPixel    : word;     { 1, 4, 8, or 24. Were always using 8 }
	    Compression     : DWORD;  { DWORD, types of compression }
	    SizeOfBitmap    : DWORD;  { DWORD, byte size of bitmap }
	    XPPM, YPPM      : DWORD;  { Horizontal/Vertical pixels/meter }
    	Colors          : DWORD;  { total number of different colors }
    	ColorsMin       : DWORD;  { number of important colors }
    end;

function SaveWinBMP3x(FileName : String; Width, Height : word; Data : TBytes;
      Colors : TWinBMPPalette; ColorCount : word) : String;
var
    FON : String;
    FH : tWinBMPFileHeader;
    BH : tWinBMPBitmapHeader;
    X, Y, LW : integer;
    F : File;
    D : TBytes;
begin
    Result := '';
    FON := Filename;
    FileName := ChangeFileExt(FON,'.bmp');
    if UpperCase(FileName) = UpperCase(FON) then
       FileName := ChangeFileExt(FON,'.new.bmp');
    LW := Width and $fffc;
    if LW <> Width then Inc(LW, 4);
    SetLength(D, Length(Data));
    for X := 0 to length(D) - 1 do
      D[X] := 0;
    { flip image }
    for Y := 0 to Height - 1 do
        for X := 0 to Width - 1 do
            D[Y * LW + X] := Data[(Height - Y - 1) * LW + X];
    { maybe compress image here }
    { set header }
    BH.HeadSize        := Sizeof(BH);
    BH.Width           := Width;
	BH.Height          := Height;
	BH.Planes          := 1;
	BH.BitsPerPixel    := 8;
	BH.Compression     := 0;
	BH.SizeOfBitmap    := Length(Data);
	BH.XPPM            := 12000;   { just guessing }
	BH.YPPM            := 12000;  { same  }
    BH.Colors          := ColorCount;
    BH.ColorsMin       := ColorCount;
    with FH do begin
        FileType := $4d42;
        Reserved := 0;
        ImageOfs := Sizeof(FH) + Sizeof(BH) + ColorCount * Sizeof(TWinBMPColor);
        FileSize := ImageOfs + BH.SizeOfBitmap;
    end;
    Assign(F, Filename);
    Rewrite(F, 1);
    BlockWrite(F, FH, Sizeof(FH));
    BlockWrite(F, BH, Sizeof(BH));
    for X := 0 to ColorCount - 1 do
        BlockWrite(F, Colors[X], Sizeof(TWinBMPColor));
    for X := 0 to Length(D) - 1 do
      BlockWrite(F, D[X], Sizeof(Byte));
    Close(F);

    Result := Filename;
end;
{$PackRecords default}


end.

