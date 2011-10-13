# .xsession
xsetroot -cursor_name left_ptr 
xrdb -merge .Xresources
# xmodmap -e "keycode  66 = XF86Search NoSymbol XF86Search"
# xmodmap -e "clear lock"
xmodmap .xmodmap
xmodmap -e "clear lock"
xmodmap -e "remove mod1 = ISO_Level3_Shift"
xmonad 

