@echo off

chcp 65001
sbt "runMain org.softlang.s2s.s2s %*"
