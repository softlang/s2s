@echo off

if exist bin\ (
  .\bin\s2s.bat "%*"
) else (
  .\target\universal\stage\bin\s2s.bat "%*"
)
