@echo off
rem See more documentation in the corresponding Shell script.

if not [%1]==[] cd /d %1

for /f "delims=" %%s in ('git log -1 --format^=""%%ci"" HEAD') do set last_commit_date_time=%%s
rem If some errors happen; e.g. Git is not installed.
if not defined last_commit_date_time exit 1
rem remove time
for /f "tokens=1 delims= " %%s in ("%last_commit_date_time%") do set last_commit_date=%%s
rem remove "-"
echo %last_commit_date:-=%
