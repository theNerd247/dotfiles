#!/bin/bash

charge=$(acpi | grep Charging)
value=$(acpi | grep "%") 
value=${value%,*}
value=${value##*, }
value=${value%%%}

color=00FFEE

output="Batt($value%)"

if [[ -z $charge   &&  $value != 100 ]] ; then 
	if [ $value -gt 66 ] ; then 
		color=00FF22
	elif [ $value -gt 33 ] ; then
		color=FFFF00
	else
		color=FF0000
	fi
fi

echo "<fc=#$color>$output</fc>"
