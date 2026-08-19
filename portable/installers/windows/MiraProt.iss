; =============================================================================
; MiraProt Inno Setup Installer Script
; =============================================================================
; Build with:
;   iscc /DAppVersion=1.0.0 MiraProt.iss
;
; Expects the built distribution in ..\..\..\dist\ with:
;   MiraProt-launcher.exe
;   shiny-app\
;   r-portable\
;   r-library\
; =============================================================================

#ifndef AppVersion
  #define AppVersion "0.0.0-dev"
#endif

[Setup]
AppName=MiraProt
AppVersion={#AppVersion}
AppVerName=MiraProt {#AppVersion}
AppPublisher=MiraProt Contributors
AppPublisherURL=https://github.com/AdSchmalen/MiraProt
DefaultDirName={autopf}\MiraProt
DefaultGroupName=MiraProt
OutputBaseFilename=MiraProt-{#AppVersion}-windows-setup
OutputDir=..\..\..\output
Compression=lzma2/max
SolidCompression=yes
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
UninstallDisplayIcon={app}\MiraProt-launcher.exe
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
WizardStyle=modern
DisableProgramGroupPage=yes
SetupLogging=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop shortcut"; GroupDescription: "Additional shortcuts:"

[Files]
; Launcher binary
Source: "..\..\..\dist\MiraProt-launcher.exe"; DestDir: "{app}"; Flags: ignoreversion

; Shiny application
Source: "..\..\..\dist\shiny-app\*"; DestDir: "{app}\shiny-app"; Flags: ignoreversion recursesubdirs createallsubdirs

; Portable R installation
Source: "..\..\..\dist\r-portable\*"; DestDir: "{app}\r-portable"; Flags: ignoreversion recursesubdirs createallsubdirs

; R package library
Source: "..\..\..\dist\r-library\*"; DestDir: "{app}\r-library"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\MiraProt"; Filename: "{app}\MiraProt-launcher.exe"; WorkingDir: "{app}"
Name: "{group}\Uninstall MiraProt"; Filename: "{uninstallexe}"
Name: "{autodesktop}\MiraProt"; Filename: "{app}\MiraProt-launcher.exe"; WorkingDir: "{app}"; Tasks: desktopicon

[Run]
Filename: "{app}\MiraProt-launcher.exe"; Description: "Launch MiraProt"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs; Name: "{app}\user_data"
Type: filesandordirs; Name: "{app}\cache"
Type: filesandordirs; Name: "{app}\logs"
