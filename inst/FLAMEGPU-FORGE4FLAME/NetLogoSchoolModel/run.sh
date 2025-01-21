#!/bin/bash

docker run --cpus=8 -it --user $UID:$UID --rm -v $(pwd):/home/docker/netlogo danielebaccega/netlogo:611 /usr/bin/bash -c "./start.sh $1 $2"
