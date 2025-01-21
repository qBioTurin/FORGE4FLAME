#!/bin/bash
occam-run -n $1 netlogo/netlogo:620 /usr/bin/bash -c "./start.sh /archive/home/dbaccega/FLAMEGPU2-F4F/NetLogoSchoolModel/${2} ${3}"
