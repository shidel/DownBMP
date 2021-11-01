{
    BSD 3-Clause License
    Copyright (c) 2021, Jerome Shidel
    All rights reserved.
}

program DownShiftBMP;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Classes, SysUtils, CustApp, IniFiles, StrUtils, Graphics, LCLType,
     IntfGraphics, fpImage;

{$I version.inc}
const
  CfgExt = '.CFG';
  VerboseOpts : array of string = ('Quite', 'Normal', 'Verbose');
  {  NoYesOpts : array of string = ('No', 'Yes', 'True'); }
  ProjectTypes : array of string = ('Detect', 'Exclude', 'Standard');

type
  PProject = ^TProject;
  TProject = record
    Parent : PProject;
    Name : String;
    Kind : integer;
    Level : integer;
    Subs : array of PProject;
  end;

  { TDownToBMP }

  TDownShiftBMP = class(TCustomApplication)
  private
  protected
      CfgFile  : TIniFile;
      Template : string;
      Verbose  : integer;
      Projects : PProject;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure SettingsDefault; virtual;
    procedure SettingsLoad; virtual;
    procedure SettingsSave; virtual;
    procedure ProcessMain; virtual;
  end;

{ TDownShiftBMP }

procedure TDownShiftBMP.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
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


  ProcessMain;

  // stop program loop
  Terminate;
end;

constructor TDownShiftBMP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  CfgFile := TIniFile.Create(LowerCase(ChangeFileExt(ExeName, CfgExt)));
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
  WriteLn;
  writeLn('Usage: ', ExtractFileName(ExeName), ' [options]');
  WriteLn;
  WriteLn(#9,'-h, --help',    #9, 'Display this help text');
  WriteLn;
{
  WriteLn(#9,'-v, --verbose', #9, 'Display more information');
  WriteLn(#9,'-q, --quite',   #9, 'Display less information');
  WriteLn;
}
end;

procedure TDownShiftBMP.SettingsDefault;
begin
  Template:='report.template';
end;

procedure TDownShiftBMP.SettingsLoad;
begin
  Template := CfgFile.ReadString('*', 'Template', Template);
  Verbose := IndexText(Trim(CfgFile.ReadString('*', 'Verbose', '')), VerboseOpts) - 1;
  if Verbose < -1 then Verbose := 0;
end;

procedure TDownShiftBMP.SettingsSave;
begin
  CfgFile.WriteString('*', 'Template', Template);
  CfgFile.WriteString('*', 'Verbose', VerboseOpts[Verbose + 1]);
end;

procedure TDownShiftBMP.ProcessMain;
var
  B : TPicture;
  F : TLazIntfImage;
  I : integer;
  X, Y : word;
  C : TFPColor;
begin
  for I := 1 to ParamCount do begin
    WriteLn(ParamStr(I));
    B := TPicture.Create;
    F := TLazIntfImage.Create(0,0);
    try
       B.LoadFromFile(ParamStr(I));
       F.LoadFromBitmap(B.Pixmap.Handle, B.PixMap.MaskHandle);
       WriteLn(F.Width, 'x', F.Height);
       for Y := 0 to F.Height - 1 do begin
           for X := 0 to F.Width - 1 do begin
               C := F.Colors[x,y];
               if C.Red + C.Blue + C.Green > 0 then
                  Write('X')
               else
                   Write(' ');
           end;
           WriteLn;
       end;

    finally
       FreeAndNil(B);
       FreeAndNil(F);
    end;

  end;
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

