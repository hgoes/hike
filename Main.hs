{-# LANGUAGE CPP #-}
module Main where

import Language.Pike.Lexer
import Language.Pike.Parser
import Language.Pike.Compiler
import Llvm.PpLlvm

import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import Control.Monad
import System.FilePath
import System.Process

data Options = Options
               { showHelp :: Bool
               , showVersion :: Bool
               , phase :: CompilationPhase
               } deriving Show

data CompilationPhase
     = GenerateLLVM
     | LinkProgram
     deriving Show

defaultOptions :: Options
defaultOptions = Options
                 { showHelp = False
                 , showVersion = False
                 , phase = LinkProgram
                 }

options :: [OptDescr (Options -> Options)]
options = [Option ['h'] ["help"] (NoArg $ \opts -> opts { showHelp = True }) "Show help for this program"
          ,Option ['v'] ["version"] (NoArg $ \opts -> opts { showVersion = True }) "Show the version number of the compiler"
          ,Option ['p'] ["phase"] (ReqArg (\str opts -> opts 
                                                          { phase = case str of
                                                               "generate-llvm-code" -> GenerateLLVM
                                                               "link-program" -> LinkProgram
                                                               _ -> error ("Unknown phase: "++str)
                                                          }) "PHASE") "Only execute a portion of the compilation process.\nSupported: generate-llvm-code and link-program"
          ]

usage :: String
usage = usageInfo "Usage: hike [OPTION...] files..." options

main = do
  rargs <- getArgs
  (opts,args) <- case getOpt Permute options rargs of
    (o,n,[]) -> return (foldl (flip id) defaultOptions o,n)
    (_,_,errs) -> do
                  hPutStr stderr ("Errors while parsing command line options:\n"++concat errs)
                  exitFailure
  when (showHelp opts) $ do
    putStr usage
    exitSuccess
  when (showVersion opts) $ do
    putStrLn $ "hike "++VERSION
    exitSuccess
  case args of
    [] -> putStr usage
    _ -> do
         mapM_ (\file -> do
                   str <- LBS.readFile file
                   case runLexer pike str of
                     Left err -> do
                       hPutStrLn stderr (show err)
                       exitFailure
                     Right tree -> case compilePike tree of
                       Left err -> do
                         hPutStrLn stderr (show err)
                         exitFailure
                       Right mod -> do
                         writeFile (replaceExtension file ".ll") (show $ ppLlvmModule mod)
                         case phase opts of
                           LinkProgram -> do
                             res <- rawSystem "llvmc" ["-clang","-o",dropExtension file,replaceExtension file ".ll"]
                             case res of
                               ExitSuccess -> return ()
                               ExitFailure n -> do
                                 hPutStrLn stderr $ "Calling llvmc failed with exit-code: "++show n
                                 exitFailure
                           _ -> return ()
               ) args


                          