@echo off
title Dashboard SINASC
color 0A
chcp 65001 >nul

echo ======================================================================
echo    DASHBOARD SINASC - DUCKDB
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
echo Iniciando o aplicativo... O navegador abrira automaticamente.
echo Para encerrar: feche a janela do navegador OU pressione Ctrl+C aqui.
echo.
echo ======================================================================

cd /d "%~dp0"

set "APP_FILE=%~dp0dashsinascparquet.r"
if not exist "%APP_FILE%" (
    echo [ERRO] Arquivo dashsinascparquet.r nao encontrado em:
    echo %APP_FILE%
    echo.
    pause
    exit /b 1
)

set "APP_FILE_R=%APP_FILE:\=/%"

if /I "%RSCRIPT%"=="Rscript" (
    Rscript -e "app <- source('%APP_FILE_R%', local = TRUE)$value; shiny::runApp(app, launch.browser = TRUE)"
) else (
    "%RSCRIPT%" -e "app <- source('%APP_FILE_R%', local = TRUE)$value; shiny::runApp(app, launch.browser = TRUE)"
)

echo.
echo ======================================================================
echo Aplicativo encerrado.
echo ======================================================================
pause