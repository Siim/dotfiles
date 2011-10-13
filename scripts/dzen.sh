#!/bin/zsh

local colorText="#999F9F"
local FG='#aaaaaa'
local BG='black'
local FONT='-*-*-*-*-*-*-10-*-*-*-*-*-*-*'
#local FONT='-*-terminus-*-*-*-*-8-*-*-*-*-*-*-*'
local ICONPATH='/home/siim/scrits/icons'
local BARH=8 # Height of gdbar
local BARW=50 # Width of gdbar
local WIFI=eth1
local MAXPOS="100"
local BARBG='#E1F2F7' # bg color gdbar
local BARFG='#999F9F' # fg color gdbar

local FW="mpc seek +5"      # 5 sec forwards
local RW="mpc seek -5"      # 5 sec backwards
local NEXTS="mpc next"      # previous song
local PREVS="mpc prev"      # next song
local TOGGS="mpc toggle"    # play/pause

TODAY=$(expr `date +'%d'` + 0)
MONTH=`date +'%m'`
YEAR=`date +'%Y'`

clock(){
  while true
      do
          echo "^tw() `date +'%a %d.%m.%Y %H:%M'`"
          # current month, highlight header and today
          #/home/siim/scripts/calendar | tail -n 7
          sleep 60;
      done 
}

#clock | dzen2 -h 11 -w 120 -ta l -fn $FONT&
#clock | dzen2 -h 11 -w 120 -x 1324 -ta c -fn $FONT&
#clock | dzen2 -h 11 -w 120 -x 0 -ta c -fn $FONT&

battery(){
  while true
    do
      acpi | sed 's/.*: \(.*[0-9][0-9]\)%.*/\1%/g'
      # acpi | awk '{ print $3 " " substr($4,0,length($4) + (($3=="Full,")?1:0))  }'
      sleep 10
    done
}

#battery | dzen2 -h 11 -w 100 -x 1180 -fn $FONT &
#battery | dzen2 -h 11 -w 100 -x 1444 -fn $FONT &
#battery | dzen2 -h 11 -w 100 -x 120 -fn $FONT &


#hnews(){
#  echo "^ca(1,/usr/bin/opera -newtab http://news.ycombinator.com) HN ^ca()"
#  rsstail -u http://news.ycombinator.com/rss -n 10 -N
#}
#
#hnews | dzen2 -h 11 -w 400 -tw 30 -x 150 -l 10 -ta c -sa l -fn $FONT &
