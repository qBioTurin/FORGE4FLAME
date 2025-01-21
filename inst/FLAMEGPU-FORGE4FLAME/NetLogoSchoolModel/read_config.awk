#!/usr/bin/awk
BEGIN{
	FS = "|"
	EXP = ""
	STR=""
	TMPSTR=""
}
{
	for(i = 1; i <= NF; i++)
	{
		switch($1)
		{
			case "experiment":
				if(i == 1)
				{
					STR = "\t<"$i" " 
				}
				else
				{
					STR = STR $i " "
					if(i == NF)
					{
						STR = STR ">"
					}
				}
				break;
			case "steppedValueSet":
				if(i == 1)
				{
					STR = "\t\t<" $i " "
				}
				else
				{
					STR = STR $i " "
					if(i == NF)
					{
						STR = STR "/>"
					}
				}
				break;
			case "enumeratedValueSet":
				if(i == 1)
				{
					STR = "\t\t<"$i" " 
				}
				else
				{
					if($i ~ /(value)/)
					{
						TMPSTR = "\n\t\t\t<value " $i "/>\n"
					}
					else
					{
						STR = STR $i " "
					}
					if(i == NF)
					{
						STR = STR ">" TMPSTR "\t\t</" $1 ">"
						TMPSTR="\t\t"
					}
				}
				break;
			default:
				if(i == 1)
				{
					STR = "\t\t<"$i">" 
				}
				else
				{
					STR = STR""$i
					if(i == NF)
					{
						STR = STR"</"$1">"
					}
				}

		}
	}
	EXP = EXP STR"\n"
}
END{
	EXP = EXP "\t</experiment>"
	print EXP
}
