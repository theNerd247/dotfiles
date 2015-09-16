#!/bin/bash

##############################
# checks for package updates
##############################

updatenum=$(pacman -Qu | wc -l)
adds=

if [[ $updatenum != 1 ]] ; then
	adds="s"
fi

echo "$updatenum update$adds available"
