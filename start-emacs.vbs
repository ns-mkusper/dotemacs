' start emacs with sane env values

' start shell
Set objShell = WScript.CreateObject("WScript.Shell")

Set Env = objShell.Environment("PROCESS")
Home = Env.Item("USERPROFILE")
Path = Env.Item("PATH")

EmacsVersion = "28.1"
EmacsHome = Home & "\scoop\apps\emacs\" & EmacsVersion & "\share\emacs\" & EmacsVersion
EmacsBin = Home & "\scoop\apps\emacs\" & EmacsVersion & "\bin\emacs.exe"
ScoopShimsPath = Home & "\scoop\shims"

' setup Emacs env
objShell.Environment("USER").Item("HOME") = EmacsHome
objShell.Environment("USER").Item("PATH") = Path & ";" & ScoopShimsPath

objShell.Run chr(34) & EmacsBin & Chr(34), 0

' hide cmd window
Set objShell = Nothing
