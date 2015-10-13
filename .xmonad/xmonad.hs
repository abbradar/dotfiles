{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns, LambdaCase #-}

import Data.List
import Data.Monoid
import qualified Data.Map as M

import Graphics.X11.Xlib
import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.TrackFloating
import XMonad.Actions.WindowGo
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.UpdatePointer
import XMonad.Prompt.Window
import XMonad.Actions.PhysicalScreens
import qualified XMonad.StackSet as W
import System.Taffybar.Hooks.PagerHints (pagerHints)

main :: IO ()
main = xmonad myConfig

myConfig =
  sc
  { modMask = mod4Mask
  , startupHook = startupHook sc <> setWMName "LG3D"
  , manageHook = manageHook sc <+> myManageHook <+> manageDocks
  , layoutHook = avoidStruts $ smartBorders $ trackFloating $ layoutHook sc
  , logHook = logHook sc <+> updatePointer Nearest
  , handleEventHook = handleEventHook sc <+> docksEventHook <+> fullscreenEventHook
  , keys = \x -> myKeys x `M.union` keys sc x
  , terminal = "urxvt"
  , workspaces = myWorkspaces
  } `additionalKeysP` myAddKeys

  where sc = ewmh $ pagerHints xfceConfig

myXPConfig = defaultXPConfig
  { font = "xft:Liberation Mono:size=14"
  , bgColor = "black"
  , height = 26
  , searchPredicate = isInfixOf
  }

-- Use physical position of screens
myKeys (XConfig {modMask = modm}) = M.fromList
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
  , ("M-b", sendMessage ToggleStruts)
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
  where shifts = [ ("4:chat", [ "Pidgin", "Gajim", "Skype" ])
                 ]
        gos = [ ("8:mail", [ "Thunderbird" ])
              , ("5:browser", [ "Firefox" ])
              , ("2:emacs", [ "Emacs" ])
              , ("7:news", [ "liferea" ])
              ]
        shiftM = concat $
                 [ [className =? x -?> doShift w | x <- cs] | (w, cs) <- shifts ] ++
                 [ [className =? x -?> doF (W.greedyView w) <+> doShift w | x <- cs] | (w, cs) <- gos ]
