#!/bin/bash

#finds all .beam files within current directory and its subdirectories and removes them.
find . -name "*.beam" -delete 

echo ".beam files have been obliterated"
