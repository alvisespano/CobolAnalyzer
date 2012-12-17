@if "%_echo%"=="" echo off

setlocal

REM ------------------------------------------------------------------
REM Configure the sample, i.e. where to find the F# compiler and C# compiler.

if "%FSHARP_HOME%"=="" ( set FSHARP_HOME="%PROGRAMFILES%\FSharpPowerPack-2.0.0.0")
if "%FSYACC%"=="" ( set FSYACC=%FSHARP_HOME%\bin\fsyacc.exe )
if "%FSLEX%"=="" ( set FSLEX=%FSHARP_HOME%\bin\fslex.exe )

REM ------------------------------------------------------------------
ECHO Building Cobol lexer and parser...

%FSLEX% --unicode -o CobolLexer.fs CobolLexer.fsl
if ERRORLEVEL 1 goto Exit

%FSYACC% --module CobolAnalyzer.Engine.CobolParser -o CobolParser.fs CobolParser.fsy
if ERRORLEVEL 1 goto Exit

	
:Exit
endlocal

exit /b %ERRORLEVEL%
