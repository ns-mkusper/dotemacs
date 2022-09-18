@ECHO OFF
SETLOCAL
SET EMACS_VERSION=28.1
SET EMACS_HOME=%HOME%\scoop\apps\emacs\%EMACS_VERSION%\share\emacs\%EMACS_VERSION%
SET EMACS_BIN=%HOME%\scoop\apps\emacs\%EMACS_VERSION%\bin\emacs.exe
REM custom HOME environment variable only for emacs and sub-procs
SET HOME=%EMACS_HOME%

START "Start Emacs" %EMACS_BIN%	
