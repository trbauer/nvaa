@ECHO OFF
@REM "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
@REM "C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build\vcvarsall.bat" x64
@REM "C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build\vcvarsall.bat" x64
set VCVARS_P_2022=C:\Program^ Files\Microsoft^ Visual^ Studio\2022\Professional\VC\Auxiliary\Build\vcvarsall.bat
set VCVARS_C_2022=C:\Program^ Files\Microsoft^ Visual^ Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat
set VCVARS_P_2019=C:\Program^ Files^ ^(x86^)\Microsoft^ Visual^ Studio\2019\Professional\VC\Auxiliary\Build\vcvarsall.bat
set VCVARS_C_2019=C:\Program^ Files^ ^(x86^)\Microsoft^ Visual^ Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat
@IF EXIST %VCVARS_P_2022% (
  @ECHO %VCVARS_P_2022%
  "%VCVARS_P_2022%" x64
) ELSE (
  @IF EXIST %VCVARS_C_2022% (
    @ECHO %VCVARS_C_2022%
    "%VCVARS_C_2022%" x64
  ) ELSE (
    @IF EXIST %VCVARS_P_2019% (
      @ECHO %VCVARS_P_2019%
      "%VCVARS_P_2019%" x64
    ) ELSE (
      @IF EXIST %VCVARS_C_2019% (
        @ECHO %VCVARS_C_2019%
        "%VCVARS_C_2019%" x64
      ) ELSE (
        @ECHO "Cannot find vcvarsall.bat"
        @EXIT /B 1
      )
    )
  )
)


@REM set PATH=%PATH%;%CUDA_PATH%\bin
