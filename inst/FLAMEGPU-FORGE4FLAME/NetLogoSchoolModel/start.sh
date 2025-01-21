#!/bin/bash
CFG=($(./sim_setup.sh -m SchoolSEIRModel.nlogo -c $1));
echo -e "executing...\n\tmkdir Results${CFG[1]}"
if ! [ -d "Results${CFG[1]}" ]; then
	mkdir Results${CFG[1]}
fi
echo $1 >> "Results${CFG[1]}/Configuration.log"
echo -e "Executing...\n\tnetlogo-headless.sh --model ${CFG[0]} --experiment ${CFG[1]} --threads $2"
#It's necessary to specify the path to netlogo-headless.sh here (for example ~/NetLogo6.1.1/netlogo-headless.sh).
NetLogo6.1.1/netlogo-headless.sh --model ${CFG[0]} --experiment ${CFG[1]} --threads $2
