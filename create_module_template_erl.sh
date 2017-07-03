#!/bin/bash
SCRIPTNAME=$0
FILENAME=$1
FILEPATH=$PWD$2

if [$# <= 2]; then 
	echo "wrong number of arguments, correct format is '$0 file_path file_name'"
else
	# create a new file 
	if [ ! -f "$FILEPATH/$FILENAME.erl" ]; then		   
		echo -e "-module($FILENAME).\n-export([]).\n\n\n" > "$FILEPATH/$FILENAME.erl"
		echo "file $FILENAME was created @ $FILEPATH."
	# 
	else	
		echo "file already exists, do you wish to override it and create a blank .erl?"
		echo "y(es) to override, n(o) avoid changes..."
		read decision
		if [ $decision == "y" ] || [ $decision == "yes" ]; then
			echo -e "-module($filename).\n-export([]).\n\n\n" > "$FILEPATH/$FILENAME.erl"
		elif [ $DECISION == "n" ] || [ $DECISION == "no" ]; then
			echo "no changes made, exiting now..."
		else
			echo "unrecognised command, please try running the script again..."
		fi	
	fi
fi


