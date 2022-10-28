#!/bin/bash

len=$1
enable_color=$2

#
# Get sound level using amixer
#
is_on=`amixer | grep -i -a6 "master" | grep -i "\[on\]" | wc -l`
if [[ is_on -eq 2 ]]
then
    left_source=`amixer | grep -i -a6 "master" | grep -i "left:" | sed -e "s/.*\[\(.*\)\%\].*/\1/"`
    right_source=`amixer | grep -i -a6 "master" | grep -i "right:" | sed -e "s/.*\[\(.*\)\%\].*/\1/"`
    sound_level=$(((left_source+right_source)/2))
    percentage=$(( (sound_level*len)/100 ))
else
	sound_level=`amixer | grep -i -a6 "master" | grep -i "Mono:" | sed -e "s/.*\[\(.*\)\%\].*/\1/"`
    percentage=$(( (sound_level*len)/100 ))
fi


#
# Characters
#
chr="="
delimiter="\\\\"
bar=""

#
# Colors
#
delimeter_color="#fff"
delimeter_mute_color="#aaa"
active_color="#5a5"
full_color="#c22"
med_color="#ed2"
low_color="#5a5"
inactive_color="#ddd"
delimeter_color_begin="<fc=$delimeter_color>"
delimeter_mute_color_begin="<fc=$delimeter_mute_color>"
active_color_begin="<fc=$active_color>"
full_color_begin="<fc=$full_color>"
med_color_begin="<fc=$med_color>"
low_color_begin="<fc=$low_color>"
inactive_color_begin="<fc=$inactive_color>"
color_end="</fc>"

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
	if [[ $enable_color == "bi" ]]
	then
		bar+=$1
	elif [[ $enable_color == "true" ]]
	then
		if [ $percentage -le 8 ]
		then
			bar+=$2
		elif [ $percentage -le 9 ]
		then
			bar+=$3
		else
			bar+=$4
		fi
	fi
	bar+=$5
	if [[ $enable_color == "bi" ]] || [[ $enable_color == "true" ]]
	then
		bar+=$6
	fi
}

for i in $(seq 0 $len)
do
    if [[ $percentage -eq -1 ]] && [[ $i -eq $len ]]
    then
        color $delimeter_mute_color_begin $delimeter_mute_color_begin $delimeter_mute_color_begin $delimeter_mute_color_begin $delimiter $color_end
        break
    fi
	if [[ $((len - i)) -eq $percentage ]]
	then
		color $delimeter_color_begin $delimeter_color_begin $delimeter_color_begin $delimeter_color_begin $delimiter $color_end
	elif [[ $((len - i)) -lt $percentage ]]
	then
		color $active_color_begin $low_color_begin $med_color_begin $full_color_begin $chr $color_end
	else
		color $inactive_color_begin $inactive_color_begin $inactive_color_begin $inactive_color_begin $chr $color_end
	fi
done

echo $bar
exit 0