{-# LANGUAGE ViewPatterns, LambdaCase #-}

import Data.List
import Control.Applicative ((<$>))
import Control.Monad
import Data.Monoid
import qualified Data.Map as M

import Control.Monad.Error
import Data.List.Split
import Data.Vinyl (rGet)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.Directory
import System.FilePath

import Graphics.X11.Xlib
import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.TrackFloating
import XMonad.Actions.WindowGo
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Actions.PhysicalScreens
import qualified XMonad.StackSet as W

import HBooru.Network
import qualified HBooru.Types as HT
import HBooru.Parsers.Gelbooru

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ myConfig { logHook = dynamicLogWithPP (prettyPrinter dbus)
                      }

myConfig =
  sc
  { modMask = mod4Mask
  , startupHook = startupHook sc <> setWMName "LG3D"
  , manageHook = manageHook sc <+> myManageHook <+> manageDocks
  , layoutHook = smartBorders $ trackFloating $ layoutHook sc
  , handleEventHook = handleEventHook sc <+> docksEventHook <+> fullscreenEventHook
  , keys = \x -> myKeys x `M.union` keys sc x
  , terminal = "urxvt"
  , workspaces = myWorkspaces
  } `additionalKeysP` myAddKeys

  where sc = ewmh xfceConfig

myXPConfig = defaultXPConfig
  { font = "xft:Liberation Mono:size=14"
  , bgColor = "black"
  , height = 26
  , searchPredicate = isInfixOf
  }

-- Use physical position of screens
myKeys conf@(XConfig {modMask = modm}) = M.fromList
  [((modm .|. mask, key), f sc)
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

myAddKeys =
  [ ("M-p", shellPrompt myXPConfig)
  , ("M-g", windowPromptGoto myXPConfig { autoComplete = Just 500000 })
  , ("M-S-i", booruPrompt myXPConfig)
  , ("M-S-g", windowPromptBring myXPConfig { autoComplete = Just 500000 })
  , ("M-S-v", runOrRaiseMaster "emacs" (className =? "Emacs"))
  , ("M-S-f", runOrRaiseMaster "firefox" (className =? "Firefox"))
  , ("M-S-t", runOrRaiseMaster "thunderbird" (className =? "Thunderbird"))
  , ("M-S-d", spawn "thunar")
  , ("M-S-l", spawn "xflock4")
  ]

myWorkspaces = [ "1"
               , "2:emacs"
               , "3"
               , "4:chat"
               , "5:browser"
               , "6"
               , "7:news"
               , "8:mail"
               , "9"
               ]

myManageHook = composeAll $ shiftM
  where shifts = [ ("4:chat", [ "Pidgin", "Skype" ])
                 ]
        gos = [ ("8:mail", [ "Thunderbird" ])
              , ("5:browser", [ "Firefox" ])
              , ("2:emacs", [ "Emacs" ])
              , ("7:news", [ "liferea" ])
              ]
        shiftM = concat $
                 [ [className =? x --> doShift w | x <- cs] | (w, cs) <- shifts ] ++
                 [ [className =? x --> doF (W.greedyView w) <+> doShift w | x <- cs] | (w, cs) <- gos ]

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = (": " ++) . pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = pangoColor "black" . pangoSanitize
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

-- H-Booru

data HBooru = HBooru

instance XPrompt HBooru where
  showXPrompt HBooru = "Gelbooru: "

booruPrompt :: XPConfig -> X ()
booruPrompt c =
  do
    mkXPrompt HBooru c (mkComplFunFromList []) showImage
  where
    showImage img = liftIO $ do
      file <- fetchImage img
      spawn $ "ristretto " ++ file

fetchImage :: String -> IO String
fetchImage (splitOn " " -> tags) = do
  let url s = [ (HT.md5 `rGet` r, HT.file_url `rGet` r) | Right r <- s ]
  Right (md5, url) <- runErrorT $ head <$> url <$> fetchTaggedPosts Gelbooru HT.XML tags
  chan <- newTChanIO
  tmp <- getTemporaryDirectory
  let path = tmp </> md5 <.> (snd $ splitExtension url)
  downloadFiles [(url, path)] chan 1
  res <- atomically $ readTChan chan >>= \case
    Failed x -> return $ Just x
    _ -> return Nothing
  maybe (return ()) (fail . show) res
  return $ show path

-- XMonad Log Applet

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
