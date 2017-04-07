#!/bin/bash
FILENAME=$1
# create a new file 
if [ ! -f $FILENAME.erl ]; then		   
	echo -e "-module($FILENAME).\n-export([]).\n\n\n" > $FILENAME.erl
	echo "File $FILENAME was created @ $PWD."
else	
	echo "File already exists, do you wish to override it and create a blank .erl?"
	echo "y(es) to override, n(o) avoid changes..."
	read DECISION
	if [ $DECISION == "y" ] || [ $DECISION == "yes" ]; then
		echo -e "-module($FILENAME).\n-export([]).\n\n\n" > $FILENAME.erl
	elif [ $DECISION == "n" ] || [ $DECISION == "no" ]; then
		echo "No changes made, exiting now..."
	else
		echo "unrecognised command, please try running the script again..."
	fi	
fi
