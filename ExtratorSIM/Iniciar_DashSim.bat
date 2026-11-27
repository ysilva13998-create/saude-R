@echo off
chcp 65001 >nul
setlocal

:: Vai para a pasta onde este .bat está salvo (<pasta_raiz_extraida>\ExtratorSIM\)
cd /d "%~dp0"

set "SCRIPT_R=%~dp0DashSimParquet.r"
if not exist "%SCRIPT_R%" (
    echo [ERRO] Arquivo DashSimParquet.r nao encontrado em: "%SCRIPT_R%"
    pause
    exit /b 1
)

:: Procura Rscript.exe no PATH ou nas instalacoes padrao do R no Windows
set "RSCRIPT_EXE="
where Rscript >nul 2>&1 && set "RSCRIPT_EXE=Rscript"

if not defined RSCRIPT_EXE (
    for /f "delims=" %%P in ('dir /b /s /o-n "C:\Program Files\R\Rscript.exe" 2^>nul') do (
        set "RSCRIPT_EXE=%%P"
        goto :found_r
    )
)

:found_r
if not defined RSCRIPT_EXE (
    echo [ERRO] Rscript.exe nao encontrado. Verifique se o R esta instalado.
    pause
    exit /b 1
)

echo Iniciando DashSimParquet.r em "%~dp0"...
"%RSCRIPT_EXE%" --vanilla -e "options(shiny.launch.browser = TRUE); shiny::runApp('%SCRIPT_R:\=/%', launch.browser = TRUE)"

if errorlevel 1 (
    echo.
    echo [ERRO] A aplicacao encerrou com erro.
    pause
)

endlocal