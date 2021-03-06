#!/bin/bash

if [[ "$3" == "" ]]; then
   echo "Arg1: Roku IP.Add.re.ss or MA:CA:DD:RE:SS"
   exit 22
fi

ROKU="$1"
echo "$ROKU" | grep ':' > /dev/null
if [[ $? == 0 ]]; then
    printf "$ROKU == "
    ROKU=$(arp -a | grep -i "$ROKU" | awk '{print $2}' | sed 's/[()]//g')
    echo "$ROKU"
fi

telnet $ROKU 8080
