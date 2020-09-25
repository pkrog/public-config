-- xmonad-contrib: https://hackage.haskell.org/package/xmonad-contrib
import XMonad
import XMonad.Hooks.DynamicLog -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe) -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-Run.html
import XMonad.Layout.IndependentScreens -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-IndependentScreens.html
--import XMonad.Util.WorkspaceCompare(getSortByXineramaRule)
import XMonad.Layout.Accordion -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-Accordion.html
import XMonad.Layout.Tabbed -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-Tabbed.html
import XMonad.Layout.LayoutCombinators hiding ( (|||) ) -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Layout-LayoutCombinators.html

-- Binding keys, see https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-EZConfig.html
--import XMonad.Util.EZConfig(additionalKeys) -- https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-EZConfig.html
import XMonad.StackSet(greedyView, shift, screen, current)
import System.IO
import Data.Map(fromList, union, Map)
import XMonad.Hooks.DynamicBars(multiPP) -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicBars.html

-- My layouts
mylayoutHook = avoidStruts (tall ||| Mirror tall ||| (tall *|* Full ) ||| Accordion ||| simpleTabbed ||| Full)
	where  tall = Tall 1 (3/100) (1/2)

main = do
    -- Get number of screens
    nScreens <- countScreens

    -- Spawn one xmobar for each screen
    xmprocs <- mapM (spawnPipe . xmobarCommand) [0..nScreens-1]

    xmonad $ docks def
      {
        borderWidth        = 4
      , normalBorderColor  = "#cacaca"
      , focusedBorderColor = "#dada1d"
      , terminal        = "urxvt"

	  -- Define layouts to use. `avoidStruts` is here to make place for xmobar.
      , layoutHook      = mylayoutHook
--      , workspaces      = withScreens nScreens (map show [1..9])
--      , workspaces      = map show [1..9]

      -- For one xmobar:
      --, logHook = dynamicLogWithPP xmobarPP
      --                  { ppOutput = hPutStrLn xmproc
      --                  , ppTitle = xmobarColor "green" "" . shorten 50
      --                  }
     , logHook                 = mapM_ dynamicLogWithPP $ zipWith pp xmprocs [0..nScreens-1]
      --, logHook                 = zipWith myLogHook xmprocs [0..nScreens-1]
      --, logHook                 = mapM_ zipWith hPutStrLn xmprocs (map show [0..nScreens-1])
      , modMask = mod4Mask     -- Rebind Mod to the Windows key
      , keys = mykeys
--      , normalBorderColor  = "#cccccc"
--      , focusedBorderColor = "#cd8b00"
      }-- `additionalKeys` myKeys

mykeys :: XConfig Layout -> Data.Map.Map (KeyMask, KeySym) (X ())
mykeys c = (myKeys c) `Data.Map.union` (XMonad.keys defaultConfig c)
  where
    myKeys conf@(XConfig {modMask = modm}) = myKeyBindings modm conf

myKeyBindings modm conf = Data.Map.fromList $
--myKeyBindings conf@(XConfig {XMonad.modMask = mod4Mask}) = Data.Map.fromList $
      [ ((modm .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
      , ((modm .|. shiftMask, xK_s), spawn "xscreensaver-command -lock && systemctl suspend")
      , ((modm .|. shiftMask, xK_h), spawn "xscreensaver-command -lock && systemctl hibernate")
      , ((modm .|. shiftMask, xK_bracketleft), spawn "sysinfo --decbright")
      , ((modm .|. shiftMask, xK_bracketright), spawn "sysinfo --incbright")
      , ((modm, xK_bracketleft), spawn "sysinfo --decvol")
      , ((modm, xK_bracketright), spawn "sysinfo --incvol")
      , ((modm, xK_minus), spawn "sysinfo --mute")
      , ((modm .|. shiftMask, xK_minus), spawn "sysinfo --switch-audio-sink")
      , ((modm, xK_equal), spawn "sysinfo --micmute")
      , ((modm, xK_d), spawn "termprg")
      ]
--      ++
--      [((m .|. modm, k), windows $ onCurrentScreen f i)
--        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
--        , (f, m) <- [(XMonad.StackSet.greedyView, 0), (XMonad.StackSet.shift, shiftMask)]]

-- xmobar command
xmobarCommand(S s) = unwords ["xmobar", "-x", show s] --, "-t", template s] where
--    template 0 = "%StdinReader%"
--    template _ = "%date%%StdinReader%"

--myLogHook :: Handle Integer -> X ()
--myLogHook handle screen = io . show s
--myLogHook handle screen = dynamicLogWithPP $ pp handle screen

myOutput screen strOut = concat [show screen, " ", strOut]

-- Pretty Printing (pp) format for xmobar
-- Status bar format for workspaces, layout indicator and window title
-- See https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html
--pp h s = marshallPP s xmobarPP {
--pp h s = XMonad.Hooks.DynamicBars.multiPP (mypp h s) (mypp h s)
pp h s = defaultPP {
      ppOutput              = hPutStrLn h . myOutput s
--    , ppSort                = getSortByXineramaRule
    , ppCurrent             = xmobarColor "#eeee00" "black" . wrap "[" "]"
    , ppHidden              = xmobarColor "white" "black"
    , ppVisible             = xmobarColor "#2390ee" "black" . wrap "(" ")"
    , ppHiddenNoWindows     = xmobarColor "#606060" "black"
    , ppUrgent              = xmobarColor "#f00000" "black" 
--  , ppSep            = ""
--  , ppWsSep          = " "
--  , ppOrder          = \(wss:layout:title:_) -> ["\NUL", title, "\NUL", wss]
--  , ppExtras =  [ show s ]

    -- Window title
    , ppTitle               = xmobarColor "green" "" . shorten 50
    }
