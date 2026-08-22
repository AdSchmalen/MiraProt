; =============================================================================
; MiraProt Inno Setup Installer Script
; =============================================================================
; Build with:
;   iscc /DDistDir="C:\path\to\MiraProt_Portable" MiraProt.iss
;
; DistDir defaults to the repository-root dist\ directory. It must contain:
;   MiraProt-launcher.exe
;   shiny-app\
;   r-portable\
;   r-library\
; =============================================================================

#ifndef DistDir
  #define DistDir SourcePath + "..\..\..\dist"
#endif

#if !FileExists(DistDir + "\VERSION")
  #error Required canonical version file is missing: "{#DistDir}\VERSION"
#endif
#define VersionHandle FileOpen(DistDir + "\VERSION")
#define CanonicalVersion FileRead(VersionHandle)
#expr FileClose(VersionHandle)
#ifdef AppVersion
  #if AppVersion != CanonicalVersion
    #error AppVersion does not match DistDir\VERSION
  #endif
#else
  #define AppVersion CanonicalVersion
#endif

; Fail at compile time with the selected input path, rather than producing an
; incomplete installer. The cache and root documentation remain optional.
#if !FileExists(DistDir + "\MiraProt-launcher.exe")
  #error Required stage-1 component is missing: "{#DistDir}\MiraProt-launcher.exe" (selected DistDir: "{#DistDir}")
#endif
#if !DirExists(DistDir + "\shiny-app")
  #error Required stage-1 component is missing: "{#DistDir}\shiny-app" (selected DistDir: "{#DistDir}")
#endif
#if !DirExists(DistDir + "\r-portable")
  #error Required stage-1 component is missing: "{#DistDir}\r-portable" (selected DistDir: "{#DistDir}")
#endif
#if !DirExists(DistDir + "\r-library")
  #error Required stage-1 component is missing: "{#DistDir}\r-library" (selected DistDir: "{#DistDir}")
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
Source: "{#DistDir}\MiraProt-launcher.exe"; DestDir: "{app}"; Flags: ignoreversion

; Shiny application
Source: "{#DistDir}\shiny-app\*"; DestDir: "{app}\shiny-app"; Flags: ignoreversion recursesubdirs createallsubdirs

; Portable R installation
Source: "{#DistDir}\r-portable\*"; DestDir: "{app}\r-portable"; Flags: ignoreversion recursesubdirs createallsubdirs

; R package library
Source: "{#DistDir}\r-library\*"; DestDir: "{app}\r-library"; Flags: ignoreversion recursesubdirs createallsubdirs

; Optional shipped cache. Preserve the stage-1 bytes as packaged seed data;
; runtime writes go to the user's application-data cache.
Source: "{#DistDir}\go-cache\*"; DestDir: "{app}\resources\go-cache"; Flags: ignoreversion recursesubdirs createallsubdirs skipifsourcedoesntexist

; Optional root documentation from the stage-1 bundle.
Source: "{#DistDir}\LICENSE.md"; DestDir: "{app}"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#DistDir}\README.md"; DestDir: "{app}"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#DistDir}\THIRD_PARTY_NOTICES.md"; DestDir: "{app}"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#DistDir}\CITATION.cff"; DestDir: "{app}"; Flags: ignoreversion skipifsourcedoesntexist
Source: "{#DistDir}\VERSION"; DestDir: "{app}"; Flags: ignoreversion

[Dirs]
; This directory is also the installed-layout signal when no seed cache ships.
Name: "{app}\resources\go-cache"

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
