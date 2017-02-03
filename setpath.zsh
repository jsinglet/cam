##
## NOTE: TO USE THIS YOU MUST source this file. Executing it alone will not
## impact your environment 

CAM=`stack path --local-install-root`/bin/

echo "Appending ${CAM} to your \$PATH variable."


export PATH="$PATH:$CAM"
