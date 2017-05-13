{-# LANGUAGE RecordWildCards
           , NamedFieldPuns
           , ViewPatterns #-}

module Main where

import Control.Monad ( guard )
import Data.Foldable ( for_ )
import Data.Function ( on )
import Data.List ( groupBy )
import Data.Maybe ( maybe, mapMaybe )
import Data.Monoid ((<>))
import System.Exit ( die )

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

    (plan, projectRoot) <-
        case optProjectRoot of
            Just root -> do
                let planJsonPath =
                        root </> distDirPrefix </> "cache" </> "plan" <.> "json"
                planJson <- decodePlanJson planJsonPath
                return (planJson, root)
            Nothing ->
                findAndDecodePlanJson

    let targets = readExeTargets optTargets

    let localPackages = Map.elems $
            Map.filter ((== UnitTypeLocal) . uType) (pjUnits plan)
        targetBinFiles = mapMaybe ciBinFile $
            targetedExeComponents targets localPackages
    for_ targetBinFiles $ \bin -> copyFileWithMetadata bin (replaceDirectory bin bindir)

targetedExeComponents :: [ExeTarget] -> [Unit] -> [CompInfo]
targetedExeComponents targets units = do
    target@ExeTarget{exeNames} <- targets
    unit@Unit{uComps}          <- units
    guard $ packageName unit == packageName target
    mapMaybe (\exeName -> Map.lookup (CompNameExe exeName) uComps) exeNames

data ExeTarget = ExeTarget
        { pkgName :: PkgName
        , exeNames :: [Text]
        }

class HasPackageName a where
    packageName :: a -> PkgName

instance HasPackageName ExeTarget where
    packageName = pkgName

instance HasPackageName Unit where
    packageName Unit{uPId = PkgId name _} = name

readExeTargets :: [Text] -> [ExeTarget]
readExeTargets =
    map (\targets -> ExeTarget (fst $ head targets) (mapMaybe snd targets))
  . groupBy ((==) `on` fst)
  . map (\targetString ->
        let (PkgName -> packageName, Text.tail -> rest) =
                Text.span (/= ':') targetString
         in if Text.null rest
            then (packageName, Nothing)
            else (packageName, Just rest))


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
            (  metavar "TARGET"
            <> help "targets of the form pkgname:exename, or just pkgname for all executables in the package"
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
