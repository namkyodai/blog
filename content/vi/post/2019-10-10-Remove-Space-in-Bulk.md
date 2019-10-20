---
layout: post
mathjax: true
title: Removing Space of files inside folders/subfolders in bulk
subtitle: Window
date: 2019-10-10T23:30:45+08:00
tags: [Window]
---

In many cases, we need to remove space in the names of files saved inside a tree of folders/subfolders. We can use following CODE to do the job.

```
:renameNoSpace [/R]
@echo off
setlocal disableDelayedExpansion
if /i "%~1"=="/R" (
set "forOption=%~1 %2"
set "inPath="
) else (
set "forOption="
if "%~1" neq "" (set "inPath=%~1\") else set "inPath="
)
for %forOption% %%F in ("%inPath%* *") do (
if /i "%~f0" neq "%%~fF" (
set "folder=%%~dpF"
set "file=%%~nxF"
setlocal enableDelayedExpansion
echo ren "!folder!!file!" "!file: =!"
ren "!folder!!file!" "!file: =!"
endlocal
)
)
```

Assume the script is called renameNoSpace.bat

Options to change are

```
renameNoSpace : (no arguments) Renames files in the current directory

renameNoSpace /R : Renames files in the folder tree rooted at the current directory

renameNoSpace myFolder : Renames files in the “myFolder” directory found in the current directory.

renameNoSpace "c:\my folder\" : Renames files in the specified path. Quotes are used because path contains a space.

renameNoSpace /R c:\ : Renames all files on the C: drive.
```

We paste renameNoSpace.bat to targetted folder/subfolders and run the file in the console.
