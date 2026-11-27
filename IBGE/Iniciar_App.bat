@echo off
title Irati Analytics 2026
color 0A
chcp 65001 >nul

echo ======================================================================
echo    IRATI ANALYTICS 2026
echo ======================================================================
echo.
echo Verificando instalacao do R...

REM ---- 1) Localiza o Rscript.exe ------------------------------------------
set "RSCRIPT="

REM Tenta o PATH primeiro
where Rscript >nul 2>nul
if %errorlevel%==0 (
    set "RSCRIPT=Rscript"
    goto :achei
)

REM Tenta caminhos comuns de instalacao
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
echo Iniciando o aplicativo... O navegador abrirah automaticamente.
echo Para encerrar: feche a janela do navegador OU pressione Ctrl+C aqui.
echo.
echo ======================================================================

REM ---- 2) Roda o app.R da pasta do .bat --------------------------------
cd /d "%~dp0"
REM Converte barras invertidas em barras normais (R nao gosta de \ no caminho)
set "APPDIR=%~dp0"
set "APPDIR=%APPDIR:\=/%"

"%RSCRIPT%" -e "shiny::runApp('%APPDIR%', launch.browser=TRUE)"

echo.
echo ======================================================================
echo Aplicativo encerrado.
echo ======================================================================
pause