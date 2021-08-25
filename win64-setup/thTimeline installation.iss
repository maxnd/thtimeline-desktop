[Setup]
AppName=Theological Timeline
AppVersion=1.0.0
AppPublisher=Massimo Nardello
AppPublisherURL=https://github.com/maxnd/thtimeline-desktop
DefaultDirName={pf}\Theological Timeline
DefaultGroupName=Theological Timeline
UninstallDisplayIcon={app}\thtimeline.exe
LicenseFile=license.rtf
OutputDir=OutputDir=\

[Files]
Source: "thtimeline.exe"; DestDir: "{app}"
Source: "thtimeline.po"; DestDir: "{app}"
Source: "thtimeline.it.po"; DestDir: "{app}"

[Icons]
Name: "{commonprograms}\Theological Timeline"; Filename: "{app}\thtimeline.exe"
Name: "{commondesktop}\Theological Timeline"; Filename: "{app}\thtimeline.exe"
Name: "{group}\Theological Timeline"; Filename: "{app}\thtimeline.exe"
