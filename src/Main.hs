{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.Except
import System.Environment
import Control.Applicative
import System.IO
import Control.Monad.IO.Class
import System.FilePath
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import           System.FilePath.Glob (glob)

import qualified Data.Text as Text
import qualified Options.Applicative as Opts
import System.Exit

import Bundle

-- | Command line options.
data Options = Options
  { optionsInputFiles  :: [FilePath]
  , optionsOutputFile  :: Maybe FilePath
  , optionsEntryPoints :: [ModuleName]
  , optionsMainModule  :: Maybe ModuleName
  , optionsNamespace   :: String
  , optionsSourceMaps  :: Bool
  } deriving Show

app :: Options -> IO ()
app Options{..}= do
  result <- runExceptT $ do
    inputFiles <- concat <$> mapM (liftIO . glob) optionsInputFiles
    when (null inputFiles) . liftIO $ do
      hPutStrLn stderr "purs bundle: No input files."
      exitFailure
    when optionsSourceMaps . liftIO $ do
      hPutStrLn stderr "purs bundle: Source maps not supported."
      exitFailure
    
    input <- forM inputFiles $ \filename -> do
      mid <- guessModuleIdentifier filename
      return (mid, filename)

    let entryIds = map (`ModuleIdentifier` Regular) optionsEntryPoints

    Bundle.bundle input entryIds optionsMainModule optionsNamespace

  case result of
    Left err -> do
      hPutStrLn stderr $ unlines $ Bundle.printErrorMessage err
      exitFailure
    Right output ->
      case optionsOutputFile of
        Just outputFile ->
          BL.writeFile outputFile output
        Nothing ->
          BL.putStr output

main :: IO ()
main = do
  Opts.execParser (Opts.info options mempty) >>= app

-- | Command line options parser.
options :: Opts.Parser Options
options = Options <$> some inputFile
                  <*> optional outputFile
                  <*> many entryPoint
                  <*> optional mainModule
                  <*> namespace
                  <*> sourceMaps
  where
  inputFile :: Opts.Parser FilePath
  inputFile = Opts.strArgument $
       Opts.metavar "FILE"
    <> Opts.help "The input .js file(s)"

  outputFile :: Opts.Parser FilePath
  outputFile = Opts.strOption $
       Opts.short 'o'
    <> Opts.long "output"
    <> Opts.help "The output .js file"

  entryPoint :: Opts.Parser Text
  entryPoint = Opts.strOption $
       Opts.short 'm'
    <> Opts.long "module"
    <> Opts.help "Entry point module name(s). All code which is not a transitive dependency of an entry point module will be removed."

  mainModule :: Opts.Parser Bundle.ModuleName
  mainModule = Opts.strOption $
       Opts.long "main"
    <> Opts.help "Generate code to run the main method in the specified module."

  namespace :: Opts.Parser String
  namespace = Opts.strOption $
       Opts.short 'n'
    <> Opts.long "namespace"
    <> Opts.value "PS"
    <> Opts.showDefault
    <> Opts.help "Specify the namespace that PureScript modules will be exported to when running in the browser."

  sourceMaps :: Opts.Parser Bool
  sourceMaps = Opts.switch $
       Opts.long "source-maps"
    <> Opts.help "Whether to generate source maps for the bundle (unsupported, always fails)."

