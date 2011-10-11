import XMonad
import XMonad.Layout
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, safeRunInTerm)
import XMonad.Util.EZConfig(additionalKeys)
import Data.Ratio ((%))
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.Circle
import XMonad.Layout.MagicFocus
import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import Data.List (isPrefixOf)
import XMonad.Prompt.AppendFile
import qualified XMonad.Prompt as XMP
import XMonad.Prompt.Email
import XMonad.Prompt.Man
import XMonad.Prompt.Input
import System.Directory

-- TODO: add startup list, e.g [(wallpaper,True),(xcalib,False), ...]
--       map exec startup progs

-- cabal install xmonad-extras to get that Volume module
import XMonad.Actions.Volume
import XMonad.Util.Dzen as UD

-- search engine imports
import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

-- Environment
import System.Environment (getEnv)

homedir :: IO String
homedir   = getEnv "HOME"

wallpaper :: String
--wallpaper = "/Pictures/Wall/skybase.jpg"
wallpaper = "/Pictures/Wall/Flow.jpeg"

setWallpaper :: String -> IO ()
setWallpaper home = spawn ("/usr/bin/hsetroot -fill " ++ home ++ wallpaper)

startDzen :: String -> String -> IO ()
startDzen path home = spawn (home ++ path)


--- Fonts and colors
myFont              = "-*-*-medium-*-*-*-10-*-*-*-*-*-iso8859-1"
myNormalBGColor     = "#2e3436"
myFocusedBGColor    = "#414141"
myNormalFGColor     = "#babdb6"
myFocusedFGColor    = "#66bed2"
myUrgentFGColor     = "#f57900"
myUrgentBGColor     = myNormalBGColor
mySeperatorColor    = "#2e3436"

main :: IO ()
main = do
    homedir >>= \path -> spawn $ "xcalib " ++ path ++ "/scripts/DELL_Studio_Monitor_Profile.icm"
    -- spawn "xcompmgr"
    homedir >>= startDzen "/scripts/startdzen.sh"
    homedir >>= setWallpaper
    home <- homedir
    projects <- getDirectoryContents "/home/siim/projects"
    xmonad =<< statusBar (show dstatusBar) myDzenPP toggleStrutsKey (conf home projects)
      
      where toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
            toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)
            
            dstatusBar = Dzen{ xpos  = 0
                             , ypos   = 0
                             , height = 11
                             , width  = 44
                             , dfont  = myFont
                            }

            myDzenPP   = defaultPP { ppCurrent  = dzenColor "white" "#000000" . pad
                                   , ppVisible  = \_ -> []
                                   , ppHidden   = \_ -> [] 
                                   , ppHiddenNoWindows = const ""
                                   , ppUrgent   = dzenColor "red" "yellow" . dzenStrip
                                   , ppWsSep    = ""
                                   , ppSep      = ""
                                   , ppLayout   = dzenColor "white" "black" .
                                                  (\ x -> case x of
                                                            "Hinted Full"           -> " [ ] "
                                                            "Hinted Tall"           -> " [→=] "
                                                            "IM Grid"               -> " [→=] "
                                                            "IM ReflectX IM Full"   -> " [|||] "
                                                            "Hinted ThreeCol"       -> " [→#] "
                                                            "Hinted Mirror ThreeCol"-> " [↓#] "
                                                            "Hinted Mirror Tall"    -> " [↓=] "
                                                            layoutName              -> if "GridRatio" `isPrefixOf` layoutName 
                                                                                          then " [#] " 
                                                                                          else " [?] "
                                                  )
                                   , ppTitle    = dzenColor "white" "black" . 
                                                  dzenEscape . 
                                                  (\title -> let tlen = 30 in case (length title) > tlen of 
                                                                                True -> (take tlen title ++ "...")
                                                                                _    ->  title 
                                                  ) . 
                                                  (\_ -> [])
                                   }



conf home projects = withUrgencyHook dzenUrgencyHook { args = ["-h", "11","-w","300","-x","1760","-fn",myFont,"-bg","#c34c2c"] } 
          $ defaultConfig { manageHook         = manageDocks <+> myManageHook <+> manageHook defaultConfig
                          , workspaces         = ["♠","♣","♦","♥","♫","⚡","⚫","☘","◢"]
                          , layoutHook         = avoidStruts $
                                                 onWorkspace "♣" progLayout $ 
                                                 onWorkspace "♫" Circle $ 
                                                 onWorkspace "☘" chatLayout $ 
                                                 onWorkspace "◢" gimpLayout myLayout
                          , terminal           = "urxvt -pe tabbed"
                          , focusedBorderColor = myFocusedFGColor
                          , normalBorderColor  = "#000000"
                          , modMask = mod4Mask
                          , keys               = \c -> (myKeys home projects) `M.union` keys defaultConfig c 
                          }

                            where progLayout = layoutHints (Mirror $ Tall 1 (3/100) (3/4))
                                           ||| layoutHints (tiled)
                                  
                                  myLayout   = layoutHints (tiled) 
                                           ||| layoutHints (smartBorders Full) 
                                           ||| layoutHints (Mirror tiled) 
                                           ||| layoutHints (Tall 1 (3/100) (1/2))
                                           ||| (GridRatio (4/3))
                                  
                                  chatLayout = withIM (1%4) (ClassName "emesene") Grid
                                  gimpLayout = withIM (0.11) (Role "gimp-toolbox") 
                                             $ reflectHoriz 
                                             $ withIM (0.15) (Role "gimp-dock") Full

                                  tiled = ThreeCol 1 (3/100) (1/2)


myManageHook = composeAll [ className =? "Vncviewer" --> doFloat
                          , title     =? "VLC media player" --> doFloat
                          , title     =? "Project Terminal" --> doF(W.shift "♣")
                          , className =? "emesene" --> doF(W.shift "☘")
                          , className =? "Emesene" --> doF(W.shift "☘")
                          , className =? "Opera" --> doF (W.shift "♠")
                          ]


--- Keys bindings
myKeys home projects = M.fromList $ [ ((0       , 0x1008ff11), spawn "aumix -v -2" >> getVolumeChannels ["Master"] >>= volumeMeter)                 -- XF86AudioLowerVolume
                           , ((0       , 0x1008ff13), spawn "aumix -v +2" >> getVolumeChannels ["Master"] >>= volumeMeter)                 -- XF86AudioRaiseVolume
                           , ((0       , 0x1008ff12), getMuteChannels ["Master"] >>= (\m -> (setMuteChannels ["Master"] $ not m) >> showMute (not m)))           -- XF86AudioMute
                           , ((0       , 0x1008ff17), spawn "mocp -f")                     -- XF86AudioNext
                           , ((0       , 0x1008ff16), spawn "mocp -r")                     -- XF86AudioPrev
                           , ((0       , 0x1008ff14), spawn "mocp -G")                     -- XF86AudioPlay
                           , ((0       , 0x1008ff41), spawn "urxvt -e wicd-curses")        -- XF86Launch1
                           , ((0       , 0x1008ff59), spawn "xrandr --auto")               -- XF86Display (detect dual monitor etc)
                             --0x1008ff1b
                           , ((0,0x1008ff1b), spawn . show $ bottomDmenu)
                           , ((mod4Mask, xK_p), spawn . show $ bottomDmenu)
                           , ((mod4Mask, xK_s), SM.submap $ searchEngineMap $ S.promptSearchBrowser P.defaultXPConfig "opera")
                           , ((mod4Mask .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)
                           , ((0, xK_Menu), S.promptSearchBrowser  P.defaultXPConfig "opera" S.google)
                           , ((mod4Mask .|. controlMask, xK_n), appendFilePrompt P.defaultXPConfig (home ++ "/NOTES"))
                           , ((mod4Mask .|. controlMask, xK_e), emailPrompt P.defaultXPConfig ["haugas@gmail.com"])
                           , ((mod4Mask .|. controlMask, xK_i),  projectPrompt P.defaultXPConfig projects)
                           , ((mod4Mask, xK_F1), manPrompt P.defaultXPConfig)
                           ]
                             -- TODO: mute icons
                             where dzen_args :: [String]
                                   dzen_args        = ["-h", "11","-w","80","-x","0","-fn",myFont,"-bg","black","-ta","c","-y","0"]

                                   showVolume :: Double -> X ()
                                   showVolume vol   =  UD.dzenWithArgs (formatVolume vol) dzen_args $ seconds 5

                                   showMute :: Bool -> X ()
                                   showMute muted   = UD.dzenWithArgs (if muted then "muted" else "unmuted") dzen_args $ seconds 2
                                   
                                   formatVolume :: Double -> String
                                   formatVolume vol = "volume: " ++ (show) vol ++"%"
                                   volumeMeter vol = UD.dzenWithArgs (loudness ++ remaining) dzen_args $ seconds 5 
                                   
                                     where loudness  = take ((round vol) `div` 5) $ repeat 'I'
                                           remaining = take (20 - (round vol) `div` 5) $ repeat '.'


searchEngineMap method = M.fromList $
     [ ((0, xK_d), method myDict)
     , ((0, xK_w), method S.wikipedia)
     ]

-- Dictionary
myDict = S.searchEngine "aare.pri.ee" "http://aare.edu.ee/dictionary.html?lang=en&meth=exact&switch=et&otsi=otsi&query="

-- single quote string
squote :: String -> String
squote s = "'" ++ s ++ "'"

-- Statusbar
data Dzen = Dzen{ xpos   :: Integer
                , ypos   :: Integer
                , height :: Integer
                , width  :: Integer
                , dfont  :: String
                }

instance Show Dzen where
  show Dzen{xpos=x,ypos=y,height=h,width=w,dfont=f} = 
    (++) ("dzen2 -ta l -fn " ++ font  ++ " ") $ unwords $ zipWith zipper ["-x ","-y ","-w ","-h "] [x,y,w,h]
      
      where zipper a b = a ++ (squote $ show b)
            font       = squote f


--- Dmenu
bottomDmenu = myDmenu True
topDmenu    = myDmenu False

myDmenu pos = Dmenu{ font       = myFont
                   , background = myNormalBGColor
                   , foreground = myNormalFGColor
                   , focusedBg  = myFocusedBGColor
                   , focusedFg  = myFocusedFGColor
                   , bottom     = pos
                   }

data Dmenu = Dmenu{ font       :: String
                  , background :: String
                  , foreground :: String
                  , focusedBg  :: String
                  , focusedFg  :: String
                  , bottom     :: Bool
                  }

instance Show Dmenu where
  show Dmenu{font=f,background=bg,foreground=fg,focusedBg=fbg,focusedFg=ffg,bottom=bt} =
    trim . unwords $ ["exec `dmenu_path | dmenu "
                     , placement bt
                     , "-fn", squote f
                     , "-nb", squote bg
                     , "-nf", squote fg
                     , "-sb", squote fbg
                     , "-sf", squote ffg
                     , "`"
                     ]
                       where placement bt | bt == True = "-b"
                                          | otherwise  = ""


-- Open django project: gvim, 3 terminals, compass
class Project a where
  ppath     :: a -> String
  ptype     :: a -> String

data ProjectConfig = ProjectConfig{ filename :: String, base_path :: String } deriving Show

instance Project ProjectConfig where
  ppath ProjectConfig{ base_path=bp, filename=f } = bp ++ takeWhile (/='.') f
  ptype ProjectConfig{ base_path=bp, filename=f } = bp ++ (tail $ dropWhile (/='.') f)

projectPrompt :: P.XPConfig -> [String] -> X ()
projectPrompt c projects = 
  inputPromptWithCompl c "Project" (XMP.mkComplFunFromList projects) ?+ \project ->
     sequence_ [mkterminal project | _ <- [1..3]] 
  >> mkgvim project                               
  >> compass_watch project                          
  >> return ()
  
  where paths                 = map path projects
        path project          = ppath ProjectConfig{base_path="/home/siim/django/", filename=project}
        mkterminal            = spawn . ("urxvt -title 'Project Terminal' -cd " ++) . path
        mkgvim                = spawn . ("urxvt -title 'Project Terminal' -e zsh -c gvim  -cd " ++) . path
        compass_watch project = spawn $ "urxvt -title 'Project Terminal' -hold -cd " ++ (path project) ++ "/static -e compass watch"
