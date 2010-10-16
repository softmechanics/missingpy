#!/usr/bin/env runhaskell

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.PackageDescription.Parse (writeHookedBuildInfo)
import qualified Distribution.Verbosity as Verbosity
import Data.List

main = defaultMainWithHooks autoconfUserHooks {
         hookedPrograms = [pyConfigProgram],
         postConf=configure
       }

pyConfigProgram = (simpleProgram "python") 

configure _ _ _ lbi = do
  mb_bi <- pyConfigBuildInfo Verbosity.normal lbi
  writeHookedBuildInfo "MissingPy.buildinfo" (mb_bi,[])

pyVersionDefines = [((<2.5), "PYTHON_PRE_2_5"),
                    ((<2.3), "PYTHON_PRE_2_3")]

definesFor v = map (\(_,d) -> "-D" ++ d) $ filter (\(f,_) -> f v) pyVersionDefines

-- Populate BuildInfo using python tool.
pyConfigBuildInfo verbosity lbi = do
  (pyConfigProg, _) <- requireProgram verbosity pyConfigProgram (withPrograms lbi)
  let python = rawSystemProgramStdout verbosity pyConfigProg
  libDir       <- python ["-c", "from distutils.sysconfig import *; print get_python_lib()"]
  incDir       <- python ["-c", "from distutils.sysconfig import *; print get_python_inc()"]
  confLibDir   <- python ["-c", "from distutils.sysconfig import *; print get_config_var('LIBDIR')"]
  pyVersionStr <- python ["-c", "import sys; sys.stdout.write(\"%d.%d\" % (sys.version_info[0], sys.version_info[1]))"]
  let libName = "python" ++ pyVersionStr
      pyVersion = read pyVersionStr
      
  return $ Just emptyBuildInfo {
    extraLibDirs   = lines confLibDir ++ lines libDir,
    includeDirs    = lines incDir ++ ["glue"],
    extraLibs      = lines libName,
    cppOptions     = definesFor pyVersion
  }
