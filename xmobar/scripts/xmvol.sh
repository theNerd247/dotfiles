#!/bin/bash

#####################################################
# displayVolume 
# 
# Displays the volume in an xmobar friendly format 
# Created for displaying the volume in xmobar
####################################################

#read from amixer and get the value of the volume

std=$(amixer get Master)
output="M"

#only run run the below code if mute is disabled
if [[ $(echo $std | grep -wc on) > 0 ]] ; then 

buff=${std#Simple*\[}
value=${buff%%%]*]}
output=$value%
#if we call for a bar then display it, other wise the default display
#is simply the number 
if [[ $1 == "-b" ]] ; then 
value=$(echo "scale=1; $value/10" | bc -q)
#create the output of a volume bar
output="|"
count=0
	while [[ $value > 0 ]] 
	do
		if [[ ${value#*.} == 5 ]] ; then 
			output=$output"+"
		else
			output=${output%%+}"="
		fi
		value=$(echo "scale=1; $value-.5" | bc -q)
		count=$((count+1))
	done
	buff=$(echo "scale=1; ($count/2)+0.5" | bc -q)
	count=${buff%%.*}
	count=$((10-$count))
while [[ $count != 0 ]] 
do
	output=$output"-"
	count=$((count-1))
done 
output=$output"|"
fi

fi

echo "Vol($output)"
