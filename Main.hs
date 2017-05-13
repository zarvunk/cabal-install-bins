{-# LANGUAGE RecordWildCards
           , NamedFieldPuns
           , ViewPatterns
           , OverloadedStrings #-}

module Main where

import Control.Monad ( guard )
import Data.Foldable ( for_ )
import Data.Maybe ( maybe, mapMaybe )
import Data.Monoid ((<>))

import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Data.Text as Text
import System.Directory ( copyFileWithMetadata )
import System.FilePath

import Options.Applicative
import Cabal.Plan

main :: IO ()
main = do
    Options{..} <- getOptions

    let distDirPrefix =
            optDistDirPrefix
        bindir =
            optBindir
        targets =
            readExeTargets optTargets

    (plan, projectRoot) <-
        case optProjectRoot of
            Just root -> do
                let planJsonPath =
                        root </> distDirPrefix </> "cache" </> "plan" <.> "json"
                planJson <- decodePlanJson planJsonPath
                return (planJson, root)
            Nothing ->
                findAndDecodePlanJson

    let localPackages = Map.elems $
            Map.filter ((== UnitTypeLocal) . uType) (pjUnits plan)
        targetBinFiles = mapMaybe ciBinFile $
            targetedExeComponents targets localPackages
    for_ targetBinFiles $ \bin ->
        copyFileWithMetadata bin (replaceDirectory bin bindir)

targetedExeComponents :: [ExeTarget] -> [Unit] -> [CompInfo]
targetedExeComponents targets units = do
    target@ExeTarget{exe} <- targets
    unit@Unit{uComps}         <- units
    guard $ packageName unit == packageName target
    case exe of
        Specific exeName ->
          maybe [] return $
            Map.lookup (CompNameExe exeName) uComps
        All ->
          Map.elems $
            Map.filterWithKey isExeComponent uComps
  where
    isExeComponent :: CompName -> CompInfo -> Bool
    isExeComponent (CompNameExe _) _ = True
    isExeComponent _               _ = False

data ExeTarget = ExeTarget
        { pkg :: PkgName
        , exe :: Exe
        }

data Exe = Specific Text | All

class HasPackageName a where
    packageName :: a -> PkgName

instance HasPackageName ExeTarget where
    packageName = pkg

instance HasPackageName Unit where
    packageName Unit{uPId = PkgId name _} = name

readExeTargets :: [Text] -> [ExeTarget]
readExeTargets =
    map $ \targetString ->
        let (PkgName -> packageName, Text.tail -> rest) =
                Text.span (/= ':') targetString
         in if rest == "*"
            then ExeTarget packageName All
            else ExeTarget packageName (Specific rest)

data Options = Options
        { optTargets :: [Text]
        , optProjectRoot :: Maybe FilePath
        , optBindir :: FilePath
        , optDistDirPrefix :: String
        }

getOptions :: IO Options
getOptions = customExecParser (prefs showHelpOnError) $
    info (helper <*> parser)
    (  fullDesc
    <> header "cabal-install-bins: install executables built by cabal new-build"
    <> progDesc ( "A workaround for the lack of `cabal new-install`. cabal-install-bins reads "
               ++ "the plan.json file produced by cabal new-build, locates the built executable "
               ++ "files, and copies them to the install directory." )
    )
  where
    parser :: Parser Options
    parser =
      Options
        <$> some ( argument (Text.pack <$> str)
            (  metavar "TARGETS"
            <> help "targets of the form pkg:exe, or pkg:* for all executables in the package"
            ) )
        <*> option (Just <$> str)
            (  long "project-root"
            <> metavar "PATH"
            <> help "path to the root directory of the cabal project (optional unless you're not in a subdirectory)"
            <> value Nothing
            )
        <*> strOption
            (  long "bindir"
            <> metavar "DIRECTORY"
            <> help "the directory to install the executables into"
            )
        <*> strOption
            (  long "distdir"
            <> metavar "DIRECTORY"
            <> help "the name of cabal-install's dist directory (defaults to 'dist-newstyle')"
            <> value "dist-newstyle"
            <> showDefault
            )
