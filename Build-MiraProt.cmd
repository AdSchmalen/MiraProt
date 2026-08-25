@echo off
setlocal
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0portable\scripts\start-build-windows.ps1" -Interactive %*
exit /b %ERRORLEVEL%
