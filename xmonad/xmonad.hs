-- xmonad-contrib: https://hackage.haskell.org/package/xmonad-contrib
import XMonad
import XMonad.Hooks.DynamicLog -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe) -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-Run.html
import XMonad.Layout.IndependentScreens
--import XMonad.Util.WorkspaceCompare(getSortByXineramaRule)

-- Binding keys, see https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-EZConfig.html
import XMonad.Util.EZConfig(additionalKeys) -- https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-EZConfig.html
import XMonad.StackSet(greedyView, shift)
import System.IO
import Data.Map(fromList, union, Map)

main = do
    -- Get number of screens
    nScreens <- countScreens

    -- Spawn one xmobar for each screen
    xmprocs <- mapM (spawnPipe . xmobarCommand) [0..nScreens-1]

    xmonad $ docks def
      {
        borderWidth     = 1
      , terminal        = "urxvt"
      , layoutHook      = avoidStruts $ layoutHook def
      , workspaces      = withScreens nScreens (map show [1..9])
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
      , ((modm, xK_equal), spawn "sysinfo --micmute")
      , ((modm, xK_d), spawn "termprg")
      ]
      ++
      [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(XMonad.StackSet.greedyView, 0), (XMonad.StackSet.shift, shiftMask)]]


myKeys conf@(XConfig {XMonad.modMask = mod4Mask}) = Data.Map.fromList $
      [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
      , ((mod4Mask .|. shiftMask, xK_s), spawn "xscreensaver-command -lock && systemctl suspend")
      , ((mod4Mask .|. shiftMask, xK_h), spawn "xscreensaver-command -lock && systemctl hibernate")
      , ((mod4Mask .|. shiftMask, xK_bracketleft), spawn "sysinfo --decbright")
      , ((mod4Mask .|. shiftMask, xK_bracketright), spawn "sysinfo --incbright")
      , ((mod4Mask, xK_bracketleft), spawn "sysinfo --decvol")
      , ((mod4Mask, xK_bracketright), spawn "sysinfo --incvol")
      , ((mod4Mask, xK_minus), spawn "sysinfo --mute")
      , ((mod4Mask, xK_equal), spawn "sysinfo --micmute")
      , ((mod4Mask, xK_d), spawn "termprg")
      ]
--      ++
--      [((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
--        | (i, k) <- zip (workspaces' ) [xK_1 .. xK_9]
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
pp h s = marshallPP s xmobarPP {
--pp h s = defaultPP {
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
