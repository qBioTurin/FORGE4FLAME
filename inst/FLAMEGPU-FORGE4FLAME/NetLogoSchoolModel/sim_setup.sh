#/bin/bash
function help()
{
	echo -e "\tUSAGE:n\t\tsim_setup.sh [OPTIONS]\n\tThe script returns the file name used to generate the new NetLogo model."
	echo -e "\tDESCRIPTION:"
	echo -e "\t\t-h Print this help."
	echo -e "\t\t-m path to the NetLogo model to execute (mandatory)."
	echo -e "\t\t-c path to the NetLogo model experiment configuration file. If -c parameter is not used, the configuration's file name will be used to specify the experiment to run (mandatory)."
	echo -e "\t\t-e name of the experiment to run."
	echo -e "\t\t-o Name of the NetLogo output file. If no name is provided, the name of the experiment is concatenated to the original NetLogo file name."
	exit 1
}

# Read command line input
while getopts m:e:c:t:h FLG
do
    case "${FLG}" in
        m) MOD=${OPTARG};;
        e) EXP=${OPTARG};;
        c) CONF=${OPTARG};;
        o) FNM=${OPTARG};;
	h) help;;
	*) help;; 
    esac
done

# Check if all required parameters have been passed
if [ -z ${MOD+x} ] || [ -z ${CONF+x} ];
then 
	help;
fi

# Check wether to use the configuration's file name or the -e input for the experiment's name
if [ -z ${EXP+x} ]
then
	# Extract configuration's file name
	EXP=$(basename $CONF)
	# Remove file extension
	EXP=${EXP%%.*}
fi

# Generate the description of the experiment
STR=$(gawk -f read_config.awk $CONF)

# Check wether to use the provided file name or to generate it
if [ -z ${FNM+x} ]
then
	# Extract configuration's file name
	FNM=$(basename $MOD)
	# Remove file extension
	FNM="$(dirname $MOD)/${FNM%%.*}_$EXP.${FNM#*.}"
fi

# Build the script file

# Remove script file if exists
if test -f "$FNM";
then
    rm $FNM
fi

SUPPORT_STR="<experiments>\n</experiments>"

# Get the number of lines before the point where to add the experiments tag
HLN=$(grep -n "link direction" $MOD | cut -f1 -d:)
HLN=$((HLN - 8))

# Get the number of lines following the experiments tag
TLN=$(expr $(wc -l $MOD | cut -f1 -d' ') - $HLN)

# Put all togheter and write it back to file
echo -e "$(head -n $HLN $MOD)\n$SUPPORT_STR\n$(tail -n $TLN $MOD)" > $FNM


# Get the number of lines before the point where to add the configuration
HLN=$(grep -n "<experiments>" $FNM | cut -f1 -d:)

# Get the number of lines following the configuration
TLN=$(expr $(wc -l $FNM | cut -f1 -d' ') - $HLN)

# Put all togheter and write it back to file
echo -e "$(head -n $HLN $FNM)\n$STR\n$(tail -n $TLN $FNM)" > $FNM

# Return the file name of the NetLogo model
echo $FNM $EXP
