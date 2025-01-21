import argparse
import json

def CreateMapEncoding(dirname_experiment):
	with open(dirname_experiment + "WHOLEmodel.json") as f:
		WHOLEmodel = json.load(f)
	
	types = WHOLEmodel["types"]
	types_IDs = {}

	for type in types:
		name = type["Name"]
		ID = int(type["ID"])

		types_IDs[name] = {"ID": ID}

	with open("MapEncoding.py", "w") as mapencoding_file:
		mapencoding_file.write("import enum\n\n")
		mapencoding_file.write("class MapEncoding(enum.Enum):\n")

		mapencoding_file.write("\tWALL = 0\n")
		mapencoding_file.write("\tWALKABLE = 1\n")
		mapencoding_file.write("\tDOOR = 2\n")
		mapencoding_file.write("\tCORRIDOR = 3\n")

		for type, data in types_IDs.items():
			mapencoding_file.write("\t" + type.upper() + " = " + str(data["ID"]) + "\n")

		mapencoding_file.write("\n")

		mapencoding_file.write("\t@classmethod\n\tdef to_str(cls, evalue):\n")
		mapencoding_file.write("\t\tif evalue is cls.DOOR:\n\t\t\treturn \"DOOR\"\n")
		mapencoding_file.write("\t\telif evalue is cls.CORRIDOR:\n\t\t\treturn \"CPOINT\"\n")

		for type, data in types_IDs.items():
			mapencoding_file.write("\t\telif evalue is cls." + type.upper() + ":\n\t\t\treturn \"" + type.upper() + "\"\n")

		mapencoding_file.write("\n")


		mapencoding_file.write("\t@classmethod\n\tdef to_code(cls, estr):\n")
		mapencoding_file.write("\t\tif estr == \"DOOR\":\n\t\t\treturn cls.DOOR\n")
		mapencoding_file.write("\t\telif estr == \"CORRIDOR\":\n\t\t\treturn cls.CPOINT\n")

		for type, data in types_IDs.items():
			mapencoding_file.write("\t\telif estr == \"" + type.upper() + "\":\n\t\t\treturn cls." + type.upper() + "\n")

		mapencoding_file.write("\n")


		mapencoding_file.write("\t@classmethod\n\tdef to_value(cls, estr):\n")
		mapencoding_file.write("\t\tif estr == \"DOOR\":\n\t\t\treturn cls.DOOR.value\n")
		mapencoding_file.write("\t\telif estr == \"CORRIDOR\":\n\t\t\treturn cls.CPOINT.value\n")

		for type, data in types_IDs.items():
			mapencoding_file.write("\t\telif estr == \"" + type.upper() + "\":\n\t\t\treturn cls." + type.upper() + ".value\n")

def main():
	parser = argparse.ArgumentParser()
	parser.add_argument('-dirname_experiment', type=str, help='Path to F4F directory')
	args = parser.parse_args()

	CreateMapEncoding("f4f/" + args.dirname_experiment + "/")

main()