rem Windows style console unit test run script
rem copy this script to bin manually and change paths
set LAZBUILD="C:\apps\lazarus\lazbuild.exe"
set TEST_PROJECT="..\sharpanaltests.lpi"
set TEST_RUNNER="test.exe"

rem Run tests
cls
call %LAZBUILD% --quiet %TEST_PROJECT%
call %TEST_RUNNER% --format=plain -a
