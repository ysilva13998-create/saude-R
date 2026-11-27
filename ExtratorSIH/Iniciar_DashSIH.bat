@echo off
title Dashboard SIH - DATASUS
color 0E
chcp 65001 >nul

echo ======================================================================
echo    DASHBOARD SIH - Sistema de Informacao Hospitalar
echo ======================================================================
echo.
echo Verificando instalacao do R...

set "RSCRIPT="
where Rscript >nul 2>nul
if %errorlevel%==0 (
    set "RSCRIPT=Rscript"
    goto :achei
)

for %%P in (
    "C:\Program Files\R\R-*\bin\Rscript.exe"
    "C:\Program Files\R\R-*\bin\x64\Rscript.exe"
    "C:\Program Files (x86)\R\R-*\bin\Rscript.exe"
    "C:\R\R-*\bin\Rscript.exe"
) do (
    for /f "delims=" %%F in ('dir /b /s %%P 2^>nul') do (
        set "RSCRIPT=%%F"
        goto :achei
    )
)

echo.
echo [ERRO] O R nao foi encontrado neste computador.
echo.
echo Baixe e instale o R em:  https://cran.r-project.org/bin/windows/base/
echo Depois execute este atalho novamente.
echo.
pause
exit /b 1

:achei
echo [OK] R encontrado em: %RSCRIPT%
echo.
echo Verificando arquivo DashSihParquet.R...

cd /d "%~dp0"

if not exist "DashSihParquet.R" (
    echo.
    echo [ERRO] Arquivo DashSihParquet.R nao encontrado nesta pasta:
    echo   %cd%
    echo.
    pause
    exit /b 1
)

echo [OK] DashSihParquet.R encontrado
echo.
echo Iniciando o dashboard SIH... O navegador abrira automaticamente.
echo Para encerrar: feche a janela do navegador OU pressione Ctrl+C aqui.
echo.
echo ======================================================================

if /I "%RSCRIPT%"=="Rscript" (
    Rscript -e "shiny::runApp('DashSihParquet.R', launch.browser=TRUE)"
) else (
    "%RSCRIPT%" -e "shiny::runApp('DashSihParquet.R', launch.browser=TRUE)"
)

echo.
echo ======================================================================
echo Dashboard SIH encerrado.
echo ======================================================================
pause