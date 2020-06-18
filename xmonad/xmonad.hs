import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)

-- Binding keys, see https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-EZConfig.html
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks def
      {
        borderWidth        = 1
      , terminal           = "urxvt"
      , layoutHook = avoidStruts  $  layoutHook def
      , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
      , modMask = mod4Mask     -- Rebind Mod to the Windows key
--      , normalBorderColor  = "#cccccc"
--      , focusedBorderColor = "#cd8b00"
      } `additionalKeys`
      [ ((mod4Mask .|. shiftMask, xK_l), spawn "i3lock")
      , ((mod4Mask .|. shiftMask, xK_s), spawn "i3lock && systemctl suspend")
      , ((mod4Mask .|. shiftMask, xK_h), spawn "i3lock && systemctl hibernate")
      , ((mod4Mask, xK_d), spawn "termprg")
      ]
