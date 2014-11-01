import Data.List
import Control.Applicative ((<$>))
import qualified Data.Map as M

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
  ewmh $ startConfig
  { modMask = mod4Mask
  , startupHook = setWMName "LG3D"
  , manageHook = manageHook startConfig <+> myManageHook <+> manageDocks
  , layoutHook = smartBorders $ trackFloating $ layoutHook startConfig
  , handleEventHook = handleEventHook startConfig <+> docksEventHook <+> fullscreenEventHook
  , keys = \x -> myKeys x `M.union` keys startConfig x
  , terminal = "urxvt"
  , workspaces = myWorkspaces
  } `additionalKeysP` myAddKeys

  where startConfig = xfceConfig

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

myManageHook = composeAll $ concat $
               [ [className =? x --> doShift w | x <- cs] | (w, cs) <- shifts ]
               ++ [ [className =? x --> doF (W.greedyView w) <+> doShift w | x <- cs] | (w, cs) <- gos ]
  where shifts = [ ("4:chat", [ "Pidgin", "Skype" ])
                 ]
        gos = [ ("8:mail", [ "Thunderbird" ])
              , ("5:browser", [ "Firefox" ])
              , ("2:emacs", [ "Emacs" ])
              , ("7:news", [ "liferea" ])
              ]

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
