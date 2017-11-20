--{-# OPTIONS_GHC -Wall #-}

import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Battery
import System.Taffybar.MPRIS2
import System.Taffybar.CPUMonitor

import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

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
      pager = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      bat = batteryBarNew defaultBatteryConfig 30
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = cpuMonitorNew cpuCfg 0.5 "cpu"
      tray = systrayNew
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager, note ]
                                        , endWidgets = [ tray, bat, clock, mem, cpu, mpris ]
                                        }
