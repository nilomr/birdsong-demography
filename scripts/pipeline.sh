#!/bin/bash

# Define the list of script names
script_names=(
  "setup/0.1-setup.R"
  "setup/0.2-song-similarity-cont.R"
  "setup/0.2-song-similarity.R"
  "setup/0.3-prepare-data-cont.R"
  "setup/0.3-prepare-data.R"
  "setup/0.4-prepare-songtypes.R"
  "setup/0.5-prepare-neighbourhood.R"
)

# Define the total number of scripts to run
total_scripts=${#script_names[@]}

# Define colors and formatting
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Define the progress bar function
progress_bar() {
  local current=$1
  local total=$2
  local width=50
  local percent=$((current * 100 / total))
  local completed=$((width * current / total))
  local remaining=$((width - completed))
  printf "\033[A\r[${GREEN}%-${completed}s${NC}%-${remaining}s] %d%%" \
    "" "" "$percent"
}

# Run the scripts and update the progress bar
for ((i=0; i<$total_scripts; i++)); do
  script="${script_names[$i]}"
  printf "Running script: %s\n" "$script"
  Rscript -e "renv::run('$script')"
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
    echo -e "\n${RED}Error running script: $script${NC}"
    exit 1
  fi
  progress_bar $((i + 1)) $total_scripts
done

# Print a newline after the progress bar is complete
echo -e "\nAll scripts completed successfully!"
