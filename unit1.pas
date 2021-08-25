// ***********************************************************************
// ***********************************************************************
// thTimeline Desktop 1.x
// Author and copyright: Massimo Nardello, Modena (Italy) 2021.
// Free software released under GPL licence version 3 or later.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version. You can read the version 3
// of the Licence in http://www.gnu.org/licenses/gpl-3.0.txt
// or in the file Licence.txt included in the files of the
// source code of this software.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// ***********************************************************************
// ***********************************************************************

unit Unit1;

{$mode objfpc}{$H+}
{$ifdef Darwin} {$modeswitch objectivec1} {$endif}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, IniFiles, LazUTF8, LazFileUtils, DefaultTranslator, Grids
  {$ifdef Darwin}, Unix, CocoaThemes, CocoaAll, CocoaTextEdits {$endif} ;

type

  { TfmMain }

  TfmMain = class(TForm)
    cbUncertain1: TCheckBox;
    cbUncertain2: TCheckBox;
    edFind: TEdit;
    edYear2: TEdit;
    edPlaces: TEdit;
    edYear1: TEdit;
    edTitle: TEdit;
    lbFind: TLabel;
    lbTags: TLabel;
    lbNotes: TLabel;
    lbPlaces: TLabel;
    lbYear2: TLabel;
    lbYear1: TLabel;
    lbTitle: TLabel;
    miHelpCopyright: TMenuItem;
    miHelp: TMenuItem;
    miItemsFind: TMenuItem;
    N4: TMenuItem;
    miItemsChronology: TMenuItem;
    N2: TMenuItem;
    mithTimeline: TMenuItem;
    miFileSaveAs: TMenuItem;
    miItemsSortTitle: TMenuItem;
    miItemsSortYear: TMenuItem;
    N3: TMenuItem;
    miItemsDelete: TMenuItem;
    miItemsNew: TMenuItem;
    miItems: TMenuItem;
    miFileClose: TMenuItem;
    N1: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    meTags: TMemo;
    meNotes: TMemo;
    miFile: TMenuItem;
    mmMenu: TMainMenu;
    odOpenDialog: TOpenDialog;
    pnFind: TPanel;
    pnLeft: TPanel;
    pnDetail: TPanel;
    pnBottom: TPanel;
    rgKind: TRadioGroup;
    sdExport: TSaveDialog;
    sdSaveDialog: TSaveDialog;
    sgData: TStringGrid;
    spData: TSplitter;
    lbNumber: TStaticText;
    procedure edFindChange(Sender: TObject);
    procedure edFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edTitleKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure meNotesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure miFileCloseClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miHelpCopyrightClick(Sender: TObject);
    procedure miItemsChronologyClick(Sender: TObject);
    procedure miItemsDeleteClick(Sender: TObject);
    procedure miItemsFindClick(Sender: TObject);
    procedure miItemsNewClick(Sender: TObject);
    procedure miItemsSortTitleClick(Sender: TObject);
    procedure miItemsSortYearClick(Sender: TObject);
    procedure rgKindClick(Sender: TObject);
    procedure sgDataSelectCell(Sender: TObject; aCol, aRow: integer;
      var CanSelect: boolean);
    procedure sgDataSelection(Sender: TObject; aCol, aRow: integer);
  private
    procedure LoadDataFromGrid;
    procedure OpenFileData;
    procedure SaveDataToFile;
    procedure SaveDataToGrid;
    procedure SetLabels;
    procedure SetNumber;

  public

  end;

var
  fmMain: TfmMain;
  myHomeDir, myConfigFile: string;
  flSaved: boolean = True;
  flLoadSave: boolean = False;
  stFileName: string;

resourcestring

  lab002a = 'Name';
  lab002b = 'Name / Title';
  lab002c = 'Title';
  lab003a = 'Year of birth';
  lab003b = 'Year';
  lab003c = 'Start year';
  lab004 = 'Uncertain';
  lab005a = 'Year of death';
  lab005b = 'End year';
  lab006 = 'Places';
  lab007 = 'Notes';
  lab008 = 'Tags';
  lab009 = 'Person';
  lab010 = 'Document';
  lab011 = 'Event';
  lab012 = 'Chronology';
  lab013 = 'Items';
  msg001 = 'Error in saving the file.';
  msg002 = 'Delete the current row?';
  msg003 = 'Error in opening the chronology.';

implementation

uses unitcopyright;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
var
  MyIni: TIniFile;
begin
{$ifdef Darwin}
  myHomeDir := GetUserDir + 'Library/Preferences/';
  myConfigFile := 'thtimeline.plist';
{$endif}
{$ifdef Windows}
  myHomeDir := GetUserDir + 'AppData\Local\thTimeline\';
  myConfigFile := 'thtimeline.ini';
{$endif}
  if DirectoryExists(myHomeDir) = False then
  begin
    CreateDirUTF8(myHomeDir);
  end;
  if FileExistsUTF8(myHomeDir + myConfigFile) then
  begin
    try
      MyIni := TIniFile.Create(myHomeDir + myConfigFile);
      fmMain.Top := MyIni.ReadInteger('thtimeline', 'top', 0);
      fmMain.Left := MyIni.ReadInteger('thtimeline', 'left', 0);
      fmMain.Width := MyIni.ReadInteger('thtimeline', 'width', 375);
      fmMain.Height := MyIni.ReadInteger('thtimeline', 'heigth', 150);
      stFileName := MyIni.ReadString('thtimeline', 'file', '');
    finally
      MyIni.Free;
    end;
  end;
  // kind, title, year1, uncertain1, year2, uncertain2, place, text, tags
  sgData.RowHeights[0] := 50;
  sgData.ColCount := 9;
  sgData.ColWidths[0] := 0;
  sgData.ColWidths[1] := 600;
  sgData.Cells[1, 0] := lab002b;
  sgData.ColWidths[2] := 0;
  sgData.ColWidths[3] := 0;
  sgData.ColWidths[4] := 0;
  sgData.ColWidths[5] := 0;
  sgData.ColWidths[6] := 0;
  sgData.ColWidths[7] := 0;
  sgData.ColWidths[8] := 0;
  sgData.FocusRectVisible := False;
  rgKind.Items.Clear;
  rgKind.Items.Add(lab009);
  rgKind.Items.Add(lab010);
  rgKind.Items.Add(lab011);
  {$ifdef Darwin}
  mithTimeline.Visible := True;
  mithTimeline.Caption := #$EF#$A3#$BF;
  miFileSave.ShortCut := 4179;
  miItemsNew.ShortCut := 4174;
  miItemsSortTitle.ShortCut := 4180;
  miItemsSortYear.ShortCut := 4185;
  miItemsFind.ShortCut := 4166;
  if isPaintDark = True then
  begin
    meNotes.Font.Color := clWhite;
    meTags.Font.Color := clWhite;
  end
  else
  begin
    meNotes.Font.Color := clBlack;
    meTags.Font.Color := clBlack;
  end;
  {$endif}
  sgData.Cells[0, 0] := lab002b;
  sgData.TitleFont.Style := [fsBold];
  lbNumber.Caption := lab013 + ': 0';
  LoadDataFromGrid;
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MyIni: TIniFile;
begin
  SaveDataToGrid;
  SaveDataToFile;
  try
    MyIni := TIniFile.Create(myHomeDir + myConfigFile);
    MyIni.WriteInteger('thtimeline', 'top', fmMain.Top);
    MyIni.WriteInteger('thtimeline', 'left', fmMain.Left);
    MyIni.WriteInteger('thtimeline', 'width', fmMain.Width);
    MyIni.WriteInteger('thtimeline', 'heigth', fmMain.Height);
    MyIni.WriteString('thtimeline', 'file', stFileName);
  finally
    MyIni.Free;
  end;
end;

procedure TfmMain.FormActivate(Sender: TObject);
begin
  {$ifdef Darwin}
  TCocoaTextView(NSScrollView(meNotes.Handle).documentView).
    setContinuousSpellCheckingEnabled(True);
  TCocoaTextView(NSScrollView(meNotes.Handle).documentView).
    textContainer.setLineFragmentPadding(15);
  TCocoaTextView(NSScrollView(meNotes.Handle).documentView).
    setAutomaticQuoteSubstitutionEnabled(True);
  TCocoaTextView(NSScrollView(meNotes.Handle).documentView).
    setSmartInsertDeleteEnabled(True);
  TCocoaTextView(NSScrollView(meNotes.Handle).documentView).
    setAutomaticLinkDetectionEnabled(False);
  TCocoaTextView(NSScrollView(meNotes.Handle).documentView).
    setImportsGraphics(False);
  {$endif}
  if ((stFileName <> '') and (FileExistsUTF8(stFileName))) then
  begin
    OpenFileData;
  end;
end;

procedure TfmMain.meNotesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ((key = 187) and (Shift = [ssMeta])) then
  begin
    if meNotes.Font.Size = 0 then
    begin
      meNotes.Font.Size := 12;
    end
    else
    begin
      if meNotes.Font.Size < 256 then
      begin
        meNotes.Font.Size := meNotes.Font.Size + 1;
      end;
    end;
    key := 0;
  end
  else
  if ((key = 189) and (Shift = [ssMeta])) then
  begin
    if meNotes.Font.Size = 0 then
    begin
      meNotes.Font.Size := 11;
    end
    else
    begin
      meNotes.Font.Size := meNotes.Font.Size - 1;
    end;
    key := 0;
  end;
end;

procedure TfmMain.edTitleKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 13 then
  begin
    SaveDataToGrid;
  end;
end;

procedure TfmMain.edFindChange(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to sgData.RowCount - 1 do
  begin
    if UTF8Pos(UTF8LowerCase(edFind.Text), UTF8LowerCase(
      sgData.Cells[1, i])) > 0 then
    begin
      sgData.Row := i;
      Break;
    end;
  end;
end;

procedure TfmMain.edFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i: integer;
begin
  if key = 27 then
  begin
    edFind.Clear;
    key := 0;
  end
  else
  if key = 13 then
  begin
    if sgData.Row < sgData.RowCount - 1 then
    begin
      for i := sgData.Row + 1 to sgData.RowCount - 1 do
      begin
        if UTF8Pos(UTF8LowerCase(edFind.Text), UTF8LowerCase(
          sgData.Cells[1, i])) > 0 then
        begin
          sgData.Row := i;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfmMain.rgKindClick(Sender: TObject);
begin
  SetLabels;
end;

procedure TfmMain.miFileOpenClick(Sender: TObject);
begin
  if odOpenDialog.Execute then
  begin
    stFileName := odOpenDialog.FileName;
    OpenFileData;
  end;
end;

procedure TfmMain.miFileSaveClick(Sender: TObject);
begin
  SaveDataToGrid;
  if stFileName <> '' then
  begin
    SaveDataToFile;
  end
  else
  begin
    if sdSaveDialog.Execute = True then
    begin
      stFileName := sdSaveDialog.FileName;
      SaveDataToFile;
    end;
  end;
end;

procedure TfmMain.miFileSaveAsClick(Sender: TObject);
begin
  SaveDataToGrid;
  if sdSaveDialog.Execute = True then
  begin
    stFileName := sdSaveDialog.FileName;
    SaveDataToFile;
  end;
end;

procedure TfmMain.miFileCloseClick(Sender: TObject);
begin
  if flSaved = True then
  begin
    stFileName := '';
    sgData.RowCount := 1;
    SetNumber;
    rgKind.ItemIndex := -1;
    edTitle.Text := '';
    edYear1.Text := '';
    cbUncertain1.Checked := False;
    cbUncertain2.Checked := False;
    edPlaces.Text := '';
    meNotes.Text := '';
    meTags.Text := '';
  end;
end;

procedure TfmMain.miItemsNewClick(Sender: TObject);
begin
  SaveDataToGrid;
  sgData.RowCount := sgData.RowCount + 1;
  sgData.Row := sgData.RowCount - 1;
  SetNumber;
  LoadDataFromGrid;
  rgKind.ItemIndex := 0;
  edTitle.SetFocus;
end;

procedure TfmMain.miItemsDeleteClick(Sender: TObject);
begin
  if sgdata.RowCount > 1 then
  begin
    if MessageDlg(msg002, mtConfirmation, [mbOK, mbCancel], 0) = mrOk then
    begin
      flLoadSave := True;
      sgData.DeleteRow(sgData.Row);
      SetNumber;
      flLoadSave := False;
      LoadDataFromGrid;
    end;
  end;
end;

procedure TfmMain.miItemsSortTitleClick(Sender: TObject);
var
  i: integer;
begin
  sgData.ColCount := 10;
  for i := 1 to sgData.RowCount - 1 do
  begin
    sgData.Cells[9, i] := sgData.Cells[1, i] + sgData.Cells[2, i];
  end;
  sgData.SortColRow(True, 9);
  sgData.ColCount := 9;
end;

procedure TfmMain.miItemsSortYearClick(Sender: TObject);
var
  i: integer;

begin
  sgData.ColCount := 10;
  for i := 1 to sgData.RowCount - 1 do
  begin
    sgData.Cells[9, i] := sgData.Cells[2, i] + sgData.Cells[1, i];
  end;
  sgData.SortColRow(True, 9);
  sgData.ColCount := 9;
end;

procedure TfmMain.miItemsFindClick(Sender: TObject);
begin
  edFind.SetFocus;
end;

procedure TfmMain.miItemsChronologyClick(Sender: TObject);
var
  myText: TMemo;
  stYears: string;
  i: integer;
begin
  try
    myText := TMemo.Create(Self);
    myText.Lines.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"> ' +
      '<html><head><meta http-equiv="content-type" content="text/html; ' +
      'charset=UTF-8"><style>' +
      'h1{font-family:"Calibri",serif;font-weight:bold;font-size:16.0pt;' +
      'text-align:center;mso-margin-bottom-alt:2cm;page-break-before:always} ' +
      'h2{font-family:"Calibri",serif;font-weight:bold;font-size:14.0pt;' +
      'text-align:left} ' +
      'h3{font-family:"Calibri",serif;font-weight:italics;font-size:12.0pt;' +
      'text-align:left} ' +
      'h4{font-family:"Calibri",serif;font-weight:italics;font-size:12.0pt;' +
      'text-align:left} ' +
      'h5{font-family:"Calibri",serif;font-weight:normal;font-size:12.0pt;' +
      'text-align:left} ' +
      'h6{font-family:"Calibri",serif;font-weight:normal;font-size:12.0pt;' +
      'text-align:left} ' +
      'td{font-family:"Calibri",serif;font-weight:normal;font-size:12.0pt;' +
      'text-align:left} ' +
      'th{font-family:"Calibri",serif;font-weight:bold;font-size:12.0pt;' +
      'text-align:left} ' +
      'ul{font-family:"Calibri",serif;font-weight:normal;font-size:12.0pt;' +
      'text-align:left} ' +
      'ol{font-family:"Calibri",serif;font-weight:normal;font-size:12.0pt;' +
      'text-align:left} ' +
      'p.MsoNormal{mso-margin-top-alt:0;mso-margin-bottom-alt:0;'
      + 'font-size:12.0pt;font-family:"Calibri",serif;} ' +
      'p.MsoFootnoteText{font-family:"Calibri",serif;font-size:10.0pt;' +
      'mso-margin-top-alt:0;mso-margin-bottom-alt:0;} ' +
      'span.MsoFootnoteReference{vertical-align:super;} ' +
      'p.MsoQuote{font-family:"Calibri",serif;margin-top:10.0pt;' +
      'margin-right:40.0pt;margin-bottom:10.0pt;margin-left:40.0pt;' +
      'font-size:10.0pt;} ' + '</style><body>');
    myText.Lines.Add('<h1>' + lab012 + '</h1>');
    for i := 1 to sgData.RowCount - 1 do
    begin
      myText.Lines.Add('<h2>' + sgData.Cells[1, i] + '</h2>');
      if sgData.Cells[0, i] = '0' then
      begin
        myText.Lines.Add('<p class=MsoNormal><i>' + lab009 + '</i></p>');
      end
      else
      if sgData.Cells[0, i] = '1' then
      begin
        myText.Lines.Add('<p class=MsoNormal><i>' + lab010 + '</i></p>');
      end
      else
      if sgData.Cells[0, i] = '2' then
      begin
        myText.Lines.Add('<p class=MsoNormal><i>' + lab011 + '</i></p>');
      end;
      stYears := sgData.Cells[2, i];
      if sgData.Cells[3, i] = '1' then
      begin
        stYears := stYears + '?';
      end;
      if sgData.Cells[4, i] <> '0' then
      begin
        stYears := stYears + ' - ' + sgData.Cells[4, i];
        if sgData.Cells[5, i] = '1' then
        begin
          stYears := stYears + '?';
        end;
      end;
      if sgData.Cells[6, i] <> '' then
      begin
        stYears := stYears + ' • ' + sgData.Cells[6, i];
      end;
      myText.Lines.Add('<p class=MsoNormal>' + stYears + '</p>');
      myText.Lines.Add('<br>');
      myText.Lines.Add('<p class=MsoNormal>' + sgData.Cells[7, i] + '</p>');
      myText.Lines.Add('<br>');
      myText.Lines.Add('<p class=MsoNormal>' + sgData.Cells[8, i] + '</p>');
    end;
    myText.Lines.Add('</body></html>');
    try
      sdExport.FileName := 'thTimeline.html';
      if sdExport.Execute = True then
      begin
        myText.Lines.SaveToFile(sdExport.FileName);
        {$ifdef Darwin}
        if FileExistsUTF8(sdExport.FileName) then
        begin
          Unix.fpSystem('open -a /Applications/Microsoft\ Word.app ' +
            sdExport.FileName);
        end;
        {$endif}
      end;
    except
      MessageDlg(msg003, mtWarning, [mbOK], 0);
    end;
  finally
    myText.Free;
  end;
end;

procedure TfmMain.miHelpCopyrightClick(Sender: TObject);
begin
  fmCopyright.ShowModal;
end;

procedure TfmMain.sgDataSelectCell(Sender: TObject; aCol, aRow: integer;
  var CanSelect: boolean);
begin
  if flLoadSave = False then
  begin
    SaveDataToGrid;
  end;
end;

procedure TfmMain.sgDataSelection(Sender: TObject; aCol, aRow: integer);
begin
  if flLoadSave = False then
  begin
    LoadDataFromGrid;
  end;
end;

procedure TfmMain.SetNumber;
begin
  lbNumber.Caption := lab013 + ': ' + IntToStr(sgData.RowCount - 1);
end;

procedure TfmMain.SetLabels;
begin
  if sgData.RowCount > 1 then
  begin
    if rgKind.ItemIndex = 1 then
    begin
      lbTitle.Caption := lab002c;
      lbYear1.Caption := lab003b;
      lbYear2.Caption := lab005b;
    end
    else
    if rgKind.ItemIndex = 2 then
    begin
      lbTitle.Caption := lab002a;
      lbYear1.Caption := lab003b;
      lbYear2.Caption := lab005b;
    end
    else
    begin
      lbTitle.Caption := lab002a;
      lbYear1.Caption := lab003a;
      lbYear2.Caption := lab005a;
    end;
  end
  else
  begin
    lbTitle.Caption := lab002b;
    lbYear1.Caption := lab003c;
    lbYear2.Caption := lab005b;
  end;
end;

procedure TfmMain.SaveDataToGrid;
var
  i: integer;
begin
  if sgData.RowCount > 1 then
  begin
    sgData.Cells[0, sgdata.Row] := IntToStr(rgKind.ItemIndex);
    sgData.Cells[1, sgdata.Row] := edTitle.Text;
    if TryStrToInt(edYear1.Text, i) = False then
    begin
      edYear1.Clear;
    end;
    if edYear1.Text <> '' then
    begin
      sgData.Cells[2, sgdata.Row] := edYear1.Text;
    end
    else
    begin
      sgData.Cells[2, sgdata.Row] := '0';
    end;
    if cbUncertain1.Checked = True then
    begin
      sgData.Cells[3, sgdata.Row] := '1';
    end
    else
    begin
      sgData.Cells[3, sgdata.Row] := '0';
    end;
    if TryStrToInt(edYear2.Text, i) = False then
    begin
      edYear2.Clear;
    end;
    if edYear2.Text <> '' then
    begin
      sgData.Cells[4, sgdata.Row] := edYear2.Text;
    end
    else
    begin
      sgData.Cells[4, sgdata.Row] := '0';
    end;
    if cbUncertain2.Checked = True then
    begin
      sgData.Cells[5, sgdata.Row] := '1';
    end
    else
    begin
      sgData.Cells[5, sgdata.Row] := '0';
    end;
    sgData.Cells[6, sgdata.Row] := edPlaces.Text;
    sgData.Cells[7, sgdata.Row] := meNotes.Text;
    sgData.Cells[8, sgdata.Row] := meTags.Text;
  end;
end;

procedure TfmMain.LoadDataFromGrid;
var
  i: integer;
begin
  if sgData.RowCount > 1 then
  begin
    if TryStrToInt(sgData.Cells[0, sgData.Row], i) = True then
    begin
      rgKind.ItemIndex := StrToInt(sgData.Cells[0, sgData.Row]);
    end
    else
    begin
      rgKind.ItemIndex := -1;
    end;
    edTitle.Text := sgData.Cells[1, sgdata.Row];
    if sgData.Cells[2, sgdata.Row] <> '0' then
    begin
      edYear1.Text := sgData.Cells[2, sgdata.Row];
    end
    else
    begin
      edYear1.Text := '';
    end;
    if sgData.Cells[3, sgdata.Row] = '1' then
    begin
      cbUncertain1.Checked := True;
    end
    else
    begin
      cbUncertain1.Checked := False;
    end;
    if sgData.Cells[4, sgdata.Row] <> '0' then
    begin
      edYear2.Text := sgData.Cells[4, sgData.Row];
    end
    else
    begin
      edYear2.Text := '';
    end;
    if sgData.Cells[5, sgdata.Row] = '1' then
    begin
      cbUncertain2.Checked := True;
    end
    else
    begin
      cbUncertain2.Checked := False;
    end;
    edPlaces.Text := sgData.Cells[6, sgdata.Row];
    meNotes.Text := sgData.Cells[7, sgdata.Row];
    meTags.Text := sgData.Cells[8, sgdata.Row];
  end;
  SetLabels;
end;

procedure TfmMain.SaveDataToFile;
var
  slFile: TStringList;
  stText: string;
  iRow, iCol: integer;
begin
  if stFileName <> '' then
    try
      try
        flLoadSave := True;
        slFile := TStringList.Create;
        stText := '';
        for iRow := 1 to sgData.RowCount - 1 do
        begin
          for iCol := 0 to sgData.ColCount - 1 do
          begin
            stText := stText + sgData.Cells[iCol, iRow] + #9;
          end;
        end;
        slFile.Text := stText;
        slFile.SaveToFile(stFileName);
      finally
        slFile.Free;
        flLoadSave := False;
      end;
    except
      MessageDlg(msg001, mtWarning, [mbOK], 0);
    end;
end;

procedure TfmMain.OpenFileData;
var
  slFile: TStringList;
  iRow, iCol: integer;
begin
  if stFileName <> '' then
    try
      try
        sgData.RowCount := 1;
        flLoadSave := True;
        slFile := TStringList.Create;
        slFile.LoadFromFile(stFileName);
        while UTF8Length(slFile.Text) > 2 do
        begin
          sgData.RowCount := sgData.RowCount + 1;
          iRow := sgData.RowCount - 1;
          begin
            for iCol := 0 to sgData.ColCount - 1 do
            begin
              sgData.Cells[iCol, iRow] :=
                UTF8Copy(slFile.Text, 1, UTF8Pos(#9, slFile.Text) - 1);
              slFile.Text := UTF8Copy(slFile.Text, UTF8Pos(#9, slFile.Text) +
                1, UTF8Length(slFile.Text));
            end;
          end;
        end;
        SetNumber;
        LoadDataFromGrid;
      finally
        slFile.Free;
        flLoadSave := False;
      end;
    except
      MessageDlg(msg001, mtWarning, [mbOK], 0);
    end;
end;

end.



