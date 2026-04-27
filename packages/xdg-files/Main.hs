{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (foldMapWithKey, fromListWith, unionsWith)
import GHC.Exts (sortWith)
import System.Directory
  ( XdgDirectory (..),
    getHomeDirectory,
    getXdgDirectory,
    listDirectory,
  )
import System.FilePath (dropExtension, makeRelative, (</>))
import System.IO.Error (catchIOError)

main :: IO ()
main = do
  home <- getHomeDirectory
  putStr . formatApplicationMap home =<< applicationMap

type ApplicationMap = Map Application [XdgEntry]

newtype Application = Application String deriving (Eq, Ord)

instance Show Application where
  show (Application s) = s

type XdgEntry = (XdgDirectory, FilePath)

applicationMap :: IO ApplicationMap
applicationMap = Map.unionsWith (++) <$> traverse applicationMapFor xdgDirectories

xdgDirectories :: [XdgDirectory]
xdgDirectories = [XdgData, XdgConfig, XdgCache, XdgState]

applicationMapFor :: XdgDirectory -> IO ApplicationMap
applicationMapFor xdgDirectory = do
  xdgPath <- getXdgDirectory xdgDirectory ""
  let fromEntry entry = (Application (dropExtension entry), [(xdgDirectory, xdgPath </> entry)])
  Map.fromListWith (++) . map fromEntry <$> listXdgEntries xdgPath

listXdgEntries :: FilePath -> IO [FilePath]
listXdgEntries path = filter visible <$> listDirectory path `catchIOError` const (pure [])

visible :: FilePath -> Bool
visible ('.' : _) = False
visible _ = True

-- * format

formatApplicationMap :: FilePath -> ApplicationMap -> String
formatApplicationMap home =
  renderApplicationMap (renderApplication (renderXdgEntries (renderLocation home)))

renderApplicationMap :: (Application -> [XdgEntry] -> [String]) -> ApplicationMap -> String
renderApplicationMap renderApp =
  unlines . intercalate [""] . Map.foldMapWithKey ((pure .) . renderApp)

renderApplication :: ([XdgEntry] -> [String]) -> Application -> [XdgEntry] -> [String]
renderApplication renderEntries application entries =
  (show application ++ ":") : map indent (renderEntries entries)

indent :: String -> String
indent = ("  " ++)

renderXdgEntries :: (XdgEntry -> String) -> [XdgEntry] -> [String]
renderXdgEntries renderEntry = map renderEntry . sortWith snd

renderLocation :: FilePath -> XdgEntry -> String
renderLocation home (xdgDirectory, path) = padRight 8 (label xdgDirectory) ++ tildify home path

padRight :: Int -> String -> String
padRight width s = s ++ replicate (max 0 (width - length s)) ' '

label :: XdgDirectory -> String
label XdgData = "data"
label XdgConfig = "config"
label XdgCache = "cache"
label XdgState = "state"

tildify :: FilePath -> FilePath -> FilePath
tildify home path =
  if relative == path then path else "~" </> relative
  where
    relative = makeRelative home path
