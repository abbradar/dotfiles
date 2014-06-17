import Data.List
import Control.Applicative
import XMonad
import XMonad.Config.Kde
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

main = xmonad myConfig

myConfig = kde4Config
  { modMask = mod4Mask
  , startupHook = setWMName "LG3D"
  , manageHook = myManageHook
  , layoutHook = smartBorders $ trackFloating $ layoutHook kde4Config
  , handleEventHook = handleEventHook kde4Config <+> docksEventHook <+> fullscreenEventHook
  } `additionalKeysP` myKeys

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

myKeys =
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
  , isFullscreen --> doFullFloat
  , kdeOverride --> doFloat
  ]
