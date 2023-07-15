#!/bin/bash

# Define the total number of scripts to run
total_scripts=3

# Define the progress bar function
progress_bar() {
  local current=$1
  local total=$2
  local width=50
  local percent=$((current * 100 / total))
  local completed=$((width * current / total))
  local remaining=$((width - completed))
  printf "\r[%-${completed}s%-${remaining}s] %d%%" \
    "" "" "$percent"
}

# Run the scripts and update the progress bar
for ((i=1; i<=$total_scripts; i++)); do
  case $i in
    1) script="0.1-setup.R";;
    2) script="0.2-song-similarity.R";;
    3) script="0.3-prepare-data.R";;
  esac
  Rscript -e 'renv::run("'"$script"'")'
  progress_bar $i $total_scripts
done

# Print a newline after the progress bar is complete
printf "\n"