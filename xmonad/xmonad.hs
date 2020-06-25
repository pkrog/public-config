import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.IndependentScreens
import XMonad.Util.WorkspaceCompare(getSortByXineramaRule)

-- Binding keys, see https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-EZConfig.html
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    nScreens <- countScreens
    -- For one xmobar:
    --xmproc <- spawnPipe "xmobar"
    xmprocs <- mapM (spawnPipe . xmobarCommand) [0..nScreens-1]
    xmonad $ docks def
      {
        borderWidth        = 1
      , terminal           = "urxvt"
      , layoutHook = avoidStruts  $  layoutHook def
--      , workspaces              = withScreens nScreens (map show [1..5])

      -- For one xmobar:
      --, logHook = dynamicLogWithPP xmobarPP
      --                  { ppOutput = hPutStrLn xmproc
      --                  , ppTitle = xmobarColor "green" "" . shorten 50
      --                  }
      , logHook                 = mapM_ dynamicLogWithPP $ zipWith pp xmprocs [0..nScreens]
      , modMask = mod4Mask     -- Rebind Mod to the Windows key
--      , normalBorderColor  = "#cccccc"
--      , focusedBorderColor = "#cd8b00"
      } `additionalKeys`
      [ ((mod4Mask .|. shiftMask, xK_l), spawn "i3lock")
      , ((mod4Mask .|. shiftMask, xK_s), spawn "i3lock && systemctl suspend")
      , ((mod4Mask .|. shiftMask, xK_h), spawn "i3lock && systemctl hibernate")
      , ((mod4Mask, xK_d), spawn "termprg")
      ]

xmobarCommand (S s) = unwords ["xmobar", "-x", show s] --, "-t", template s] where
--    template 0 = "%StdinReader%"
--    template _ = "%date%%StdinReader%"

pp h s = marshallPP s xmobarPP {
--    ppCurrent           = color "yellow",
    ppCurrent = dzenColor "red" "#efebe7",
    ppVisible = wrap "[" "]",
    ppSort    = getSortByXineramaRule,
--    ppVisible           = color "white",
--    ppHiddenNoWindows   = color dark,
    ppUrgent            = color "red",
--    ppSep               = "",
--    ppOrder             = \(wss:layout:title:_) -> ["\NUL", title, "\NUL", wss],
    ppOutput            = hPutStrLn h,
    ppTitle             = xmobarColor "green" "" . shorten 50
    }
    where color c = xmobarColor c ""
