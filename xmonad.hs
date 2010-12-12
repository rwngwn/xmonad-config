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






-- Scratchpads
scratchpads =
	[ NS "stardict" "stardict" (className =? "Stardict")
		(customFloating $ W.RationalRect (2/5) (2/5) (1/2) (1/2))
	] where role = stringProperty "WM_WINDOW_ROLE"
		
main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
	xmproc <- spawnPipe "stalonetray -bg '#000000' --no-shrink -i 8 --geometry 10x1 --sticky &>/dev/null &"
	xmonad $ defaultConfig
		{ manageHook 	= namedScratchpadManageHook scratchpads <+> manageDocks <+> manageHook defaultConfig
		, layoutHook 	= avoidStruts  $  layoutHook defaultConfig
		, borderWidth	= 2 
		, terminal	= "gnome-terminal"
		, modMask	= mod4Mask
		, logHook	= dynamicLogWithPP $ xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "black" "" . shorten 75
			}
		} `additionalKeysP`
		[ ("M-C-b", spawn "firefox -P default")
		, ("M-C-s", namedScratchpadAction scratchpads "stardict")
 		]		





