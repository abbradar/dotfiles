{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Taffybar.SimpleConfig
import System.Taffybar.Widget.SNITray
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.FreedesktopNotifications
import System.Taffybar.Widget.MPRIS2
import System.Taffybar.Widget.CPUMonitor
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Information.Memory

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

main :: IO ()
main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      workspaces = workspacesNew defaultWorkspacesConfig 
      note = notifyAreaNew defaultNotificationConfig
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = cpuMonitorNew cpuCfg 0.5 "cpu"
      tray = sniTrayNew
  simpleTaffybar defaultSimpleTaffyConfig { startWidgets = [ workspaces, note ]
                                          , endWidgets = [ tray, clock, mem, cpu, mpris ]
                                          }
