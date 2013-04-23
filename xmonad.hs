import Data.List
import XMonad
import XMonad.Core
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.TrackFloating
import XMonad.Actions.WindowGo
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

main = xmonad =<< xmobar myConfig

myConfig = desktopConfig {
    modMask = mod4Mask
  , startupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
  , terminal = "urxvt"
  , manageHook = manageHook desktopConfig <+> myManageHook
  , layoutHook = smartBorders $ trackFloating $ layoutHook desktopConfig
  , handleEventHook = handleEventHook desktopConfig <+> docksEventHook <+> fullscreenEventHook
} `additionalKeysP` myKeys

myXPConfig = defaultXPConfig {
    font = "xft:Droid Sans Mono:pixelsize=12"
  , bgColor = "black"
  , searchPredicate = isInfixOf
}

lockScreen = spawn "systemctl --user start lockscreen.target"
powerSuspend = lockScreen >> spawn "dbus-send --system --print-reply --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Suspend"
powerHibernate = lockScreen >> spawn "dbus-send --system --print-reply --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Hibernate"

screenshotFile = "$(xdg-user-dir PICTURES)/Screenshots/$(date +%Y-%m-%d-%H%M%S).png"
screenshotVar = "SCROTFILE=\"" ++ screenshotFile ++ "\""
screenshotFolder = screenshotVar ++ "; SCROTDIR=$(dirname \"$SCROTFILE\")"
screenshotReady = screenshotFolder ++ "; mkdir \"$SCROTDIR\""

myKeys =
  [ ("M-S-q", spawn "systemctl --user exit")
  , ("M-p", shellPrompt myXPConfig)
  , ("M-g", windowPromptGoto myXPConfig { autoComplete = Just 500000 })
  , ("M-S-g", windowPromptBring myXPConfig { autoComplete = Just 500000 })
  , ("M-S-v", runOrRaiseMaster "gvim" (className =? "Gvim"))
  , ("M-S-f", runOrRaiseMaster "firefox" (className =? "Firefox"))
  , ("M-S-d", runOrRaiseMaster "thunar" (className =? "Thunar"))
  , ("M-S-l", lockScreen)
  , ("M-s", powerSuspend)
  , ("M-S-s", powerHibernate)
  , ("<XF86Sleep>", powerHibernate)
  , ("<XF86PowerOff>", powerSuspend)
  , ("<Print>", spawn (screenshotReady ++ "; import -window root \"$SCROTFILE\""))
  , ("C-<Print>", spawn (screenshotReady ++ "; ~/dotfiles/bin/shot-active.sh \"$SCROTFILE\""))
  , ("M1-<Print>", spawn (screenshotReady ++ "; import \"$SCROTFILE\""))
  , ("<XF86TouchpadToggle>", spawn "synclient TouchpadOff=$(synclient | egrep -q 'TouchpadOff += 1' && echo 0 || echo 1)")
  ]

myManageHook = composeAll
  [ className =? "qemu-system-i386" --> doFloat
  , isFullscreen --> doFullFloat
  ]
