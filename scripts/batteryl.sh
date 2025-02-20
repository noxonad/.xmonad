#!/bin/bash

#
# Input
#
battery=$1
len=$2
enable_color=$3

if [ -z "$battery" ]; then
  echo "Please provide a battery"
  exit 1
fi

if [ -z "$len" ]; then
  echo "Please provide a length"
  exit 3
fi

#
# Characters
#
chr="="
delimiter="\\\\"

#
# Colors
#
delimiter_color="#fff"
delimiter_discharging_color="#E4BB13"
delimiter_charging_color="#38AF1C"
active_color="#689349"
full_color="#689349"
med_color="#ed2"
low_color="#c22"
inactive_color="#ddd"
delimiter_color_begin="<fc=$delimiter_color>"
delimiter_discharging_color_begin="<fc=$delimiter_discharging_color>"
delimiter_charging_color_begin="<fc=$delimiter_charging_color>"
active_color_begin="<fc=$active_color>"
full_color_begin="<fc=$full_color>"
med_color_begin="<fc=$med_color>"
low_color_begin="<fc=$low_color>"
inactive_color_begin="<fc=$inactive_color>"
color_end="</fc>"

#
# Final bar
#
bar=""

percentage=$(upower -i /org/freedesktop/UPower/devices/battery_${battery} | grep -m1 -i "percentage:" | sed -e "s/.*\ \s\(.*\)%.*/\1/")
percentage=$((((len * ((percentage + len - 1) / len) * len)) / 100))

#
# Blinking color
# param 1: First color
# param 2: Second color
# param 3: Character
# param 4: color end
#
function blink() {
  time=$(date +%s)
  if (($time % 2 == 1)); then
    bar+=$1
  else
    bar+=$2
  fi
  bar+=$3
  bar+=$4
}

#
# Color a string depending on type of coloring that is set now
# 1 param: bi color
# 2 param: color when high battery
# 3 param: color when medium battery
# 4 param: color when low battery
# 5 param: string to color
# 6 param: end of color
# return:  colored string
#
function color() {
  if [[ $enable_color == "bi" ]]; then
    bar+=$1
  elif [[ $enable_color == "true" ]]; then
    if [ $percentage -le 3 ]; then
      bar+=$2
    elif [ $percentage -le 6 ]; then
      bar+=$3
    else
      bar+=$4
    fi
  fi
  bar+=$5
  if [[ $enable_color == "bi" ]] || [[ $enable_color == "true" ]]; then
    bar+=$6
  fi
}

for i in $(seq 0 $len); do
  if [[ $((len - i)) -eq $percentage ]]; then
    if [[ $(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -m1 -i "state:" | sed -e "s/.*\ \s\(.*\)/\1/") == "discharging" ]]; then
      blink $delimiter_color_begin $delimiter_discharging_color_begin $delimiter $color_end
    elif [[ $(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -m1 -i "state:" | sed -e "s/.*\ \s\(.*\)/\1/") == "charging" ]]; then
      blink $delimiter_color_begin $delimiter_charging_color_begin $delimiter $color_end
    else
      color $delimiter_color_begin $delimiter_color_begin $delimiter_color_begin $delimiter_color_begin $delimiter $color_end
    fi
  elif [[ $((len - i)) -lt $percentage ]]; then
    color $active_color_begin $low_color_begin $med_color_begin $full_color_begin $chr $color_end
  else
    color $inactive_color_begin $inactive_color_begin $inactive_color_begin $inactive_color_begin $chr $color_end
  fi
done

echo $bar
exit 0

