import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Actions.WindowGo(runOrRaise)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName(setWMName)







-- Scratchpads
scratchpads =
	[ NS "stardict" "stardict" (className =? "Stardict")
		(customFloating $ W.RationalRect (2/5) (2/5) (1/2) (1/2))
	] where role = stringProperty "WM_WINDOW_ROLE"
		
main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
	xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
		{ manageHook 	= namedScratchpadManageHook scratchpads <+> manageDocks <+> manageHook defaultConfig
		, startupHook	= setWMName "LG3D"
		, layoutHook 	= avoidStruts  $  layoutHook defaultConfig
		, borderWidth	= 2 
		, workspaces	= [ "Web", "IM", "ADS", "TMP" ] ++ map show [5..9]
		, terminal	= "gnome-terminal"
		, modMask	= mod4Mask
		, logHook	= dynamicLogWithPP $ xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "black" "" . shorten 50
			, ppHiddenNoWindows	= id
			}
		} `additionalKeysP`
		[ ("M-C-b", spawn "firefox -P default")
		, ("M-C-s", namedScratchpadAction scratchpads "stardict")
 		]		





