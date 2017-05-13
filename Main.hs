{-# LANGUAGE RecordWildCards
           , NamedFieldPuns
           , ViewPatterns #-}

module Main where

import Control.Monad ( guard )
import Data.Foldable ( for_ )
import Data.Function ( on )
import Data.List ( groupBy )
import Data.Maybe ( maybe, fromMaybe, mapMaybe )
import System.Exit ( die )

import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Data.Text as Text
import System.Directory ( copyFileWithMetadata )
import System.FilePath

import Cabal.Plan

main :: IO ()
main = do
    Options{..} <- return (Options [] Nothing Nothing Nothing)

    let distDirPrefix =
            fromMaybe "dist-newstyle" optDistDirPrefix

    (plan, projectRoot) <-
        case optProjectRoot of
            Just root -> do
                let planJsonPath =
                        root </> distDirPrefix </> "cache" </> "plan" <.> "json"
                planJson <- decodePlanJson planJsonPath
                return (planJson, root)
            Nothing ->
                findAndDecodePlanJson

    let targets = readExeTargets $ map Text.pack optTargets


    bindir <- maybe
                  getConfiguredBindir
                  return
                  optBindir

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

data Options = Options
        { optTargets :: [String]
        , optProjectRoot :: Maybe FilePath
        , optBindir :: Maybe FilePath
        , optDistDirPrefix :: Maybe String
        }

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

getConfiguredBindir :: IO FilePath
getConfiguredBindir =
    die $ "Haven't yet figured out how to get the configured bindir.\n"
       ++ "For now you have to tell me via the `--bindir` flag."
