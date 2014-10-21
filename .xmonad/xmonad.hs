import Data.List
import Control.Applicative ((<$>))
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
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Actions.PhysicalScreens

main :: IO ()
main = xmonad myConfig

myConfig =
  ewmh $ startConfig
  { modMask = mod4Mask
  , startupHook = setWMName "LG3D"
  , manageHook = manageHook startConfig <+> myManageHook <+> manageDocks
  , layoutHook = smartBorders $ trackFloating $ layoutHook startConfig
  , handleEventHook = handleEventHook startConfig <+> docksEventHook <+> fullscreenEventHook
  , keys = \x -> myKeys x `M.union` keys startConfig x
  , terminal = "urxvt"
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

myManageHook = composeAll
  [ className =? "qemu-system-i386" --> doFloat
  ]
