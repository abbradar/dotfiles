{-# LANGUAGE ViewPatterns, LambdaCase #-}

import Data.List
import Control.Applicative ((<$>))
import Control.Monad
import Data.Monoid
import qualified Data.Map as M

import Control.Monad.Error
import Data.List.Split
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
import XMonad.Actions.UpdatePointer
import XMonad.Prompt.Window
import XMonad.Actions.PhysicalScreens
import qualified XMonad.StackSet as W

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ myConfig { logHook = logHook myConfig >> dynamicLogWithPP (prettyPrinter dbus)
                      }
myConfig =
  sc
  { modMask = mod4Mask
  , startupHook = startupHook sc <> setWMName "LG3D"
  , manageHook = manageHook sc <+> myManageHook <+> manageDocks
  , layoutHook = smartBorders $ trackFloating $ layoutHook sc
  , logHook = logHook sc >> updatePointer Nearest
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

myManageHook = composeOne $
               [ transience
               ] ++ shiftM ++
               [ title =? "Star Wars: Knights of the Old Republic" -?> doFloat
               , isFullscreen -?> doFullFloat
               ]
  where shifts = [ ("4:chat", [ "Pidgin", "Skype" ])
                 ]
        gos = [ ("8:mail", [ "Thunderbird" ])
              , ("5:browser", [ "Firefox" ])
              , ("2:emacs", [ "Emacs" ])
              , ("7:news", [ "liferea" ])
              ]
        shiftM = concat $
                 [ [className =? x -?> doShift w | x <- cs] | (w, cs) <- shifts ] ++
                 [ [className =? x -?> doF (W.greedyView w) <+> doShift w | x <- cs] | (w, cs) <- gos ]

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
