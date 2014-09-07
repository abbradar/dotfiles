import Data.List
import Control.Applicative ((<$>))
import qualified Data.Map as M

import Graphics.X11.Xlib
import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.TrackFloating
--import XMonad.Layout.Fullscreen
import XMonad.Actions.WindowGo
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Actions.PhysicalScreens

main :: IO ()
main = xmonad myConfig

startConfig = kde4Config

myConfig = startConfig
  { modMask = mod4Mask
  , startupHook = setWMName "LG3D"
  , manageHook = myManageHook <+> manageDocks -- <+> fullscreenManageHook
  , layoutHook = smartBorders $ trackFloating $ layoutHook startConfig
  , handleEventHook = handleEventHook kde4Config <+> docksEventHook <+> fullscreenEventHook
  , keys = \x -> myKeys x `M.union` keys startConfig x
  } `additionalKeysP` myAddKeys

myXPConfig = defaultXPConfig
  { font = "xft:Monospace:size=14"
  , bgColor = "black"
  , height = 26
  , searchPredicate = isInfixOf
  }

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
  override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
  wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
  return $ maybe False (elem $ fromIntegral override) wt

myKeys conf@(XConfig {modMask = modm}) = M.fromList
  [((modm .|. mask, key), f sc)
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

myAddKeys =
  [ ("M-p", shellPrompt myXPConfig)
  , ("M-S-p", spawn "krunner")
  , ("M-g", windowPromptGoto myXPConfig { autoComplete = Just 500000 })
  , ("M-S-g", windowPromptBring myXPConfig { autoComplete = Just 500000 })
  , ("M-S-v", runOrRaiseMaster "emacs" (className =? "Emacs"))
  , ("M-S-f", runOrRaiseMaster "firefox" (className =? "Firefox"))
  ]

myManageHook = composeAll
  [ not <$> (className =? "krunner") --> manageHook kde4Config
  , className =? "qemu-system-i386" --> doFloat
  , className =? "Knotes" --> doFloat
  , className =? "Klipper" --> doFloat
  , kdeOverride --> doFloat
  ]
