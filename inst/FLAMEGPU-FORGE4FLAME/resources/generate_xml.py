import argparse
import math
import json
import sys
import graph
import os
import os
import numpy as np
import pandas as pd
from MapEncoding import *
from itertools import chain

def distribution(type, types, a, b, flow_time):
	random_number = 0

	if type == types["Deterministic"]:
		random_number = a
	
	if type == types["Exponential"]:
		random_number = -a * np.log(1 - np.random.uniform(0, 1, 1)[0])
	
	if type == types["Uniform"]:
		random_number = a + (b - a) * np.random.uniform(0, 1, 1)[0]
	
	if type == types["Truncated Positive Normal"]:
		random_number = a + np.random.normal() * b

	return 1.0 if random_number < 1.0 and flow_time else random_number

def parse_distribution(time, dist):
	# Deterministic or exponential: n
	a = time
	b = 0.0

	# Uniform: a = n; b = m
	# Truncated Positive Normal: Mean = n; Sd = m
	if dist == 'Uniform' or dist == 'Truncated Positive Normal':
		a, b = time.split(";")

		a = a.split("=")[1]
		b = b.split("=")[1]

	return float(a), float(b)

def obtain_rooms(WHOLEmodel):
	rooms = WHOLEmodel["rooms"]
	rooms_info = {}

	for room in rooms:
		name = room["Name"]
		ID = int(room["ID"])
		length = float(room["l"])
		width = float(room["w"])
		height = float(room["h"])

		r, g, b, _ = room["colorFill"].replace("rgba(", "").replace(")", "").replace(" ", "").split(",")

		rooms_info[name] = {"ID": ID, "length": length, "width": width, "height": height, "RGB": (int(float(r)), int(float(g)), int(float(b)))}
		
	return rooms_info

def obtain_canvas_dimension(WHOLEmodel):
	canvas = WHOLEmodel["canvasDimension"]
	canvas_dimensions = {}

	for c in canvas:
		canvas_dimensions["x"] = c["canvasWidth"] / 10
		canvas_dimensions["z"] = c["canvasHeight"] / 10

	return canvas_dimensions

def obtain_areas(WHOLEmodel):
	areas = WHOLEmodel["areas"]
	areas_dict = {}

	for w in areas:
		name = w["Name"]
		ID = int(w["ID"])
		r, g, b, _ = w["Color"].replace("rgba(", "").replace(")", "").replace(" ", "").split(",")

		areas_dict[name] = {"ID": ID, "RGB": (int(float(r)), int(float(g)), int(float(b)))}

	return areas_dict

def read_model(room_file, rooms, areas, y_offset, floor, WHOLEmodel, floor_name, types_IDs):

	roomsINcanvas = WHOLEmodel["roomsINcanvas"]

	color = WHOLEmodel["color"][0]
	resources = WHOLEmodel["resources"]

	rooms_whatif = pd.DataFrame(WHOLEmodel["rooms_whatif"])

	local_graph = graph.SpatialGraph()

	roomsINcanvas =  [room for room in roomsINcanvas if room["CanvasID"] == floor_name]


	y = y_offset * floor
	for room in roomsINcanvas:
		x = room["x"] - 1
		z = room["y"] - 1

		center_x = room["center_x"] - 1
		center_z = room["center_y"] - 1

		room_name = room["Name"]

		length = rooms[room_name]["length"]
		width = rooms[room_name]["width"]
		height = rooms[room_name]["height"]

		type = room["type"]
		area = room["area"]
		door = room["door"]

		ventilation = 0
		rooms_whatif_ventilation = rooms_whatif.query("type == \"" + type + "\" and area == \"" + area + "\"")
		if not rooms_whatif_ventilation.empty:
			ventilation = int(rooms_whatif_ventilation.iloc[0]["ventilation"]) / 3600

		quanta_concentration = 0

		color_id = np.where(np.array(list(rooms.keys()), dtype=str) == room_name)[0][0]
		if color == "Area":
			color_id = np.where(np.array(list(areas.keys()), dtype=str) == area)[0][0]
		if color == "Type":
			color_id = np.where(np.array(list(types_IDs.keys()), dtype=str) == type)[0][0]

		resources_room = {}
		waiting_room = {}
		agents_specific_resources = {}
		if len(resources) != 0 and type != "Spawnroom" and type != "Fillingroom" and type != "Stair" and type != "Waitingroom":
			resources_room = resources[type + "-" + area]["roomResource"]
			waiting_room = resources[type + "-" + area]["waitingRooms"]
		
		for res in resources_room:
			if res["room"] == room_name:
				for key, value in res.items():
					if key != "room":
						agents_specific_resources[key] = value

		resources_dataframe = pd.DataFrame.from_dict(agents_specific_resources, orient = "index")
		waiting_room_dataframe = pd.DataFrame.from_dict(waiting_room)
		if len(waiting_room_dataframe) != 0:
				waiting_room_dataframe = waiting_room_dataframe.set_index("Agent")

		if door == "bottom":
			yaw = 0
			x_offset = 0
			z_offset = 0
			x_door = center_x
			z_door = z + width + 1
			dimension_x = length + 1
			dimension_z = width + 1
		elif door == "left":
			yaw = math.pi / 2
			x_offset = width + 1
			z_offset = 0
			x_door = x
			z_door = center_z
			dimension_x = width + 1
			dimension_z = length + 1
		elif door == "top":
			yaw = math.pi
			x_offset = length + 1
			z_offset = width + 1
			x_door = center_x
			z_door = z
			dimension_x = length + 1
			dimension_z = width + 1
		elif door == "right":
			yaw = 3 * math.pi / 2
			x_offset = 0
			z_offset = length +1
			x_door = x + width + 1
			z_door = center_z
			dimension_x = width + 1
			dimension_z = length + 1
		else:
			yaw = 0
			x_offset = 0
			z_offset = 0

		if type != "Spawnroom":
			room_file.write("\t<xagent>\n")
			room_file.write("\t\t<name>" + ("room" if type != "Fillingroom" else "fillingroom") + "</name>\n")
			room_file.write("\t\t<x>" + str(x + x_offset + 0.5) + "</x>\n")
			room_file.write("\t\t<y>" + str(y) + "</y>\n")
			room_file.write("\t\t<z>" + str(z + z_offset + 0.5) + "</z>\n")
			room_file.write("\t\t<length_obj>" + str(rooms[room_name]["length"] + 1) + "</length_obj>\n")
			room_file.write("\t\t<width_obj>" + str(rooms[room_name]["width"] + 1) + "</width_obj>\n")
			room_file.write("\t\t<height_obj>" + str(rooms[room_name]["height"]) + "</height_obj>\n")
			room_file.write("\t\t<yaw>" + str(yaw) + "</yaw>\n")
			room_file.write("\t\t<init_room>0</init_room>\n")
			room_file.write("\t\t<area>" + str(areas[area]["ID"]) + "</area>\n")
			room_file.write("\t\t<color_id>" + str(color_id) + "</color_id>\n")
			if type != "Fillingroom":
				room_file.write("\t\t<volume>" + str(length * width * height) + "</volume>\n")
				room_file.write("\t\t<ventilation>" + str(ventilation) + "</ventilation>\n")
				room_file.write("\t\t<room_quanta_concentration>" + str(quanta_concentration) + "</room_quanta_concentration>\n")
				room_file.write("\t\t<x_center>" + str(center_x)  + "</x_center>\n")
				room_file.write("\t\t<y_center>" + str(y) + "</y_center>\n")
				room_file.write("\t\t<z_center>" + str(center_z) + "</z_center>\n")
			room_file.write("\t</xagent>\n")
			
		if door != "none":
			local_graph.add_vertex(x_door, y, z_door, [x_door, z_door], [x_door, z_door], MapEncoding.DOOR, areas[area]["ID"], yaw, 0, 0, pd.DataFrame(), pd.DataFrame())
			local_graph.add_vertex(center_x, y, center_z, [int(x), int(z)], [int(x + dimension_x), int(z + dimension_z)], MapEncoding.to_code(type.upper()), areas[area]["ID"], yaw, length, width, resources_dataframe, waiting_room_dataframe)
	
	nodesINcanvas = WHOLEmodel["nodesINcanvas"]
	nodesINcanvas = [node for node in nodesINcanvas if node["CanvasID"] == floor_name]
	for node in nodesINcanvas:
		x = node["x"] - 1
		z = node["y"] - 1

		local_graph.add_vertex(x, y, z, [x - 1, z - 1], [x + 1, z + 1], MapEncoding.CORRIDOR, -1, 0, 1, 1, pd.DataFrame(), pd.DataFrame())

	local_graph.init_edges(np.array(WHOLEmodel["matricesCanvas"][floor_name]))

	return local_graph


def generate_xml(input_file, random_seed, rooms, areas, pedestrian_names, agents, ensemble, checkpoint,
				 env_dims, total_agents_overestimation, sol_length, y_offset, dirname_experiment,
				 WHOLEmodel, autogenerated_defines, autogenerated_variables_names, days, floors_IDs, types_IDs,
				 steps_in_a_day, steps_in_a_hour, steps_in_a_minute, init_week_day, start_step_time, num_counters):
	
	with open(input_file, "w") as configuration_file:

		graphs = []
		with open("rooms_file.xml", "w") as room_file:
			room_file.write("<rooms>\n")
			for key, value in floors_IDs.items():
				local_graph = read_model(room_file, rooms, areas, y_offset, value["order"], WHOLEmodel, key, types_IDs)
				graphs.append(local_graph)
			room_file.write("</rooms>\n")

		autogenerated_variables_names.write("#define LENGTH_OBJ \"length_obj\"\n")
		autogenerated_variables_names.write("#define WIDTH_OBJ \"width_obj\"\n")
		autogenerated_variables_names.write("#define HEIGHT_OBJ \"height_obj\"\n")
		autogenerated_variables_names.write("#define YAW \"yaw\"\n")
		autogenerated_variables_names.write("#define VOLUME \"volume\"\n")
		autogenerated_variables_names.write("#define X_CENTER \"x_center\"\n")
		autogenerated_variables_names.write("#define Y_CENTER \"y_center\"\n")
		autogenerated_variables_names.write("#define Z_CENTER \"z_center\"\n")
		autogenerated_variables_names.write("#define VENTILATION \"ventilation\"\n")
		autogenerated_variables_names.write("#define ROOM_QUANTA_CONCENTRATION \"room_quanta_concentration\"\n")
		autogenerated_variables_names.write("#define INIT_ROOM \"init_room\"\n")
		autogenerated_variables_names.write("#define AREA \"area\"\n")
		autogenerated_variables_names.write("#define COLOR_ID \"color_id\"\n")

		num_floors = len(floors_IDs.items())

		if num_floors > 1:
			final_graph = graph.SpatialGraph.merge_graphs(graphs)
		else:
			final_graph = graphs[0]
			
		final_graph.save("G.txt")

		vlist = sorted(chain.from_iterable(final_graph.vertices.values()), key = lambda v: v.id)
		index2coord = np.full((3, len(vlist)), -1, dtype=int)
		coord2index = np.full((num_floors, env_dims[2], env_dims[0]), -1, dtype=int)
		adjmatrix = np.zeros((len(vlist), len(vlist)), dtype=int)
		node_type = np.full((len(vlist)), -1, dtype=int)
		node_yaw = np.zeros((len(vlist)), dtype=float)
		node_length = np.zeros((len(vlist)), dtype=float)
		node_width = np.zeros((len(vlist)), dtype=float)
		rooms_quanta_concentration = np.zeros((len(vlist)), dtype=float)
		extern_node = 0
		entrance_y_coords = 0

		autogenerated_defines.write("#define V " + str(len(vlist)) + "\n")
		autogenerated_defines.write("#define ENV_DIM_X " + str(env_dims[0]) + "\n")
		autogenerated_defines.write("#define ENV_DIM_Y " + str(env_dims[1]) + "\n")
		autogenerated_defines.write("#define ENV_DIM_Z " + str(env_dims[2]) + "\n")
		autogenerated_defines.write("#define FLOORS " + str(num_floors) + "\n")
		autogenerated_defines.write("#define YOFFSET " + str(y_offset) + "\n\n")

		autogenerated_defines.write("#define TOTAL_AGENTS_OVERESTIMATION " + str(total_agents_overestimation) + "\n\n")

		autogenerated_defines.write("#define DAYS " + str(days) + "\n\n")
		
		for v in vlist:
			index2coord[0][v.id] = v.coords.x
			index2coord[1][v.id] = v.coords.y
			index2coord[2][v.id] = v.coords.z

			for i in range(int(v.coords.northwest[0]), int(v.coords.southeast[0]+1)):
				for j in range(int(v.coords.northwest[1]), int(v.coords.southeast[1]+1)):
					coord2index[int(v.coords.y/y_offset)][j][i] = v.id

			node_type[v.id] = v.type.value
			node_yaw[v.id] = v.yaw
			node_length[v.id] = v.length
			node_width[v.id] = v.width

			if v.type == MapEncoding.SPAWNROOM:
				starting_x_range = [v.coords.northwest[0], v.coords.southeast[0]]
				starting_z_range = [v.coords.northwest[1], v.coords.southeast[1]]
				extern_node = v.id
				entrance_y_coords = v.coords.y

		distributions = {"No swab": -1, "No quarantine": -1, "Deterministic": 0, "Exponential": 1, "Uniform": 2, "Truncated Positive Normal": 3}

		compartments = {"S": 0, "E": 1, "I": 2, "R": 3, "D": 4}

		agents_whatif = pd.DataFrame(WHOLEmodel["agents_whatif"])

		disease = WHOLEmodel["disease"]
		compartmental_model_name = disease["Name"][0]

		disease_states = len(compartments.keys())
		compartmental_model = np.zeros(disease_states, dtype=int)

		a, b = parse_distribution(disease["gamma_time"][0], disease["gamma_dist"][0])
		mean_infection_days_time = [int(np.round(1 / a)), int(np.round(1 / b)) if b > 0 else 0]
		mean_infection_days_dist = distributions[disease["gamma_dist"][0]]

		mean_incubation_days_time = [0, 0]
		mean_incubation_days_dist = 0
		mean_fatality_days_time = [0, 0]
		mean_fatality_days_dist = 0
		mean_end_of_immunization_days_time = [0, 0]
		mean_end_of_immunization_days_dist = 0
		if "E" in compartmental_model_name:
			autogenerated_defines.write("#define INCUBATION\n")
			a, b = parse_distribution(disease["alpha_time"][0], disease["alpha_dist"][0])
			mean_incubation_days_time = [int(np.round(1 / a)), int(np.round(1 / b)) if b > 0 else 0]
			mean_incubation_days_dist = distributions[disease["alpha_dist"][0]]

		if "D" in compartmental_model_name:
			autogenerated_defines.write("#define FATALITY\n")
			a, b = parse_distribution(disease["lambda_time"][0], disease["lambda_dist"][0])
			mean_fatality_days_time = [int(np.round(1 / a)), int(np.round(1 / b)) if b > 0 else 0]
			mean_fatality_days_dist = distributions[disease["lambda_dist"][0]]

		if compartmental_model_name.count("S") == 2:
			autogenerated_defines.write("#define REINFECTION\n")
			a, b = parse_distribution(disease["nu_time"][0], disease["nu_dist"][0])
			mean_end_of_immunization_days_time = [int(np.round(1 / a)), int(np.round(1 / b)) if b > 0 else 0]
			mean_end_of_immunization_days_dist = distributions[disease["nu_dist"][0]]

		autogenerated_defines.write("\n\n")

		whatif = pd.DataFrame(WHOLEmodel["whatif"])

		perc_inf_df = pd.DataFrame(data={'day': range(1, days+1), "percentage_infected": np.zeros(days)})
		outside_contagion_file = "f4f/" + dirname_experiment + whatif.iloc[0]["outside_contagion_file"]
		if whatif.iloc[0]["outside_contagion_file"] != "":
			perc_inf_df = pd.read_csv(outside_contagion_file, dtype=float, index_col="day")

			if len(perc_inf_df) < days:
				print("ERROR: The dataframe outputted by the macro model must have at least " + str(days) + " rows. At the moment it has " + str(len(perc_inf_df)) + " rows.")
				sys.exit(-1)

			perc_inf_df = perc_inf_df.iloc[:days, :]

		mask_types = {"No mask": 0, "Surgical mask": 1, "FFP2 mask": 2}

		areas_len_flows = len(areas.keys())

		total_number_of_agents_types = len(agents.keys())

		env_flow = np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_flow_area = np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_flow_distr = np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_flow_distr_firstparam = np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_flow_distr_secondparam = np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_activity_type = np.full((total_number_of_agents_types, 7, sol_length), 1, dtype=float)
		env_hours_schedule = np.zeros((total_number_of_agents_types, 7, sol_length), dtype=int)
		env_birth_rates_distr =  np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_birth_rates_distr_firstparam =  np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_birth_rates_distr_secondparam =  np.full((total_number_of_agents_types, 7, sol_length), -1, dtype=int)
		env_events = np.full((total_number_of_agents_types, sol_length), -1, dtype=int)
		env_events_area = np.full((total_number_of_agents_types, sol_length), -1, dtype=int)
		env_events_cdf = np.zeros((total_number_of_agents_types, sol_length), dtype=float)
		env_events_activity = np.zeros((total_number_of_agents_types, sol_length), dtype=float)
		env_events_distr = np.full((total_number_of_agents_types, sol_length), -1, dtype=int)
		env_events_distr_firstparam = np.full((total_number_of_agents_types, sol_length), -1, dtype=int)
		env_events_distr_secondparam = np.full((total_number_of_agents_types, sol_length), -1, dtype=int)
		env_mask_type = np.full(total_number_of_agents_types+1, mask_types["No mask"], dtype=int)
		env_mask_fraction = np.zeros(total_number_of_agents_types+1, dtype=float)
		env_vaccination_fraction = np.zeros(total_number_of_agents_types+1, dtype=float)
		env_vaccination_efficacy = np.zeros(total_number_of_agents_types+1, dtype=float)
		env_swab_distr = np.full(total_number_of_agents_types+1, -1, dtype=int)
		env_swab_distr_firstparam = np.full(total_number_of_agents_types+1, -1, dtype=float)
		env_swab_distr_secondparam = np.full(total_number_of_agents_types+1, -1, dtype=float)
		env_quarantine_days_distr = np.full(total_number_of_agents_types+1, -1, dtype=int)
		env_quarantine_days_distr_firstparam = np.full(total_number_of_agents_types+1, -1, dtype=int)
		env_quarantine_days_distr_secondparam = np.full(total_number_of_agents_types+1, -1, dtype=int)
		env_quarantine_swab_days_distr = np.full(total_number_of_agents_types+1, -1, dtype=int)
		env_quarantine_swab_days_distr_firstparam = np.full(total_number_of_agents_types+1, -1, dtype=float)
		env_quarantine_swab_days_distr_secondparam = np.full(total_number_of_agents_types+1, -1, dtype=float)
		env_room_for_quarantine_type = np.full(total_number_of_agents_types+1, -1, dtype=int)
		env_room_for_quarantine_area = np.full(total_number_of_agents_types+1, -1, dtype=int)
		env_external_screening_first = np.full(total_number_of_agents_types+1, -1, dtype=float)
		env_external_screening_second = np.full(total_number_of_agents_types+1, -1, dtype=float)
		
		initial_infected_type = whatif["initial_infected_type"][0]

		initial_infected_agents = np.zeros(total_number_of_agents_types+1, dtype=int)
		if initial_infected_type == "Global":
			initial_infected_agents = np.full(total_number_of_agents_types+1, int(whatif["initial_infected"][0]), dtype=int)
			initial_infected_agents[total_number_of_agents_types] = 0
		elif initial_infected_type == "Random":
			initial_infected_agents[total_number_of_agents_types] = int(whatif["initial_infected"][0])

		total_number_of_agents = 0
		number_of_agents_by_type = np.zeros(total_number_of_agents_types+1, dtype=int)
		for index, (agent_name, agent_info) in enumerate(agents.items()):
			n = int(agent_info["NumAgent"][0])
			number_of_agents_by_type[index] = n
			total_number_of_agents = total_number_of_agents + n
		
		number_of_agents_by_type[total_number_of_agents_types] = total_number_of_agents

		global_resources = np.full(len(vlist), 2147483647, dtype=int)
		global_resources_counter = np.zeros(len(vlist), dtype=int)
		specific_resources =  np.full((total_number_of_agents_types, len(vlist)), 2147483647, dtype=int)
		specific_resources_counter = np.zeros((total_number_of_agents_types, len(vlist)), dtype=int)
		alternative_resources_type = np.full((total_number_of_agents_types, len(vlist)), -2, dtype=int)
		alternative_resources_area = np.full((total_number_of_agents_types, len(vlist)), -2, dtype=int)

		with open("agents_file.xml", "w") as agent_file:
			agent_file.write("<agents>\n")
			nawar = 0
			agents_count = 0
			agent_type_idx = 0
			for agent_name, agent_info in agents.items():
				n = int(agent_info["NumAgent"][0])

				agent_whatif = agents_whatif.query("name == \"" + agent_name + "\"")

				room_for_quarantine = agent_whatif.iloc[0]["room_for_quarantine"]

				env_mask_type[agent_type_idx] = mask_types[agent_whatif.iloc[0]["mask"]]
				env_mask_fraction[agent_type_idx] = float(agent_whatif.iloc[0]["mask_fraction"])
				env_vaccination_fraction[agent_type_idx] = float(agent_whatif.iloc[0]["vaccination"])
				env_vaccination_efficacy[agent_type_idx] = float(agent_whatif.iloc[0]["vaccination_efficacy"])
				env_swab_distr[agent_type_idx] = distributions[agent_whatif.iloc[0]["swab_dist"]]
				env_swab_distr_firstparam[agent_type_idx] = float(agent_whatif.iloc[0]["swab_a"])
				env_swab_distr_secondparam[agent_type_idx] = float(agent_whatif.iloc[0]["swab_b"])
				env_quarantine_days_distr[agent_type_idx] = distributions[agent_whatif.iloc[0]["quarantine_days_dist"]]
				env_quarantine_days_distr_firstparam[agent_type_idx] = int(agent_whatif.iloc[0]["quarantine_days_a"])
				env_quarantine_days_distr_secondparam[agent_type_idx] = int(agent_whatif.iloc[0]["quarantine_days_b"])
				env_quarantine_swab_days_distr[agent_type_idx] = distributions[agent_whatif.iloc[0]["quarantine_swab_days_dist"]]
				env_quarantine_swab_days_distr_firstparam[agent_type_idx] = float(agent_whatif.iloc[0]["quarantine_swab_days_a"])
				env_quarantine_swab_days_distr_secondparam[agent_type_idx] = float(agent_whatif.iloc[0]["quarantine_swab_days_b"])
				env_room_for_quarantine_type[agent_type_idx] =  MapEncoding.to_value(room_for_quarantine.split("-")[0].upper())
				env_room_for_quarantine_area[agent_type_idx] = areas[room_for_quarantine.split("-")[1]]["ID"]
				env_external_screening_first[agent_type_idx] = float(agent_whatif.iloc[0]["external_screening_first"])
				env_external_screening_second[agent_type_idx] = float(agent_whatif.iloc[0]["external_screening_second"])
				
				if initial_infected_type == "Different for each agent":
					initial_infected_agents[agent_type_idx] = int(agent_whatif.iloc[0]["initial_infected"])

				days_of_a_week = {"Monday": 0, "Tuesday": 1, "Wednesday": 2, "Thursday": 3, "Friday": 4, "Saturday": 5, "Sunday": 6}

				deterministic_flow = pd.DataFrame(agent_info["DeterFlow"]).sort_values(by=["FlowID","Flow"])
				random_flow = agent_info["RandFlow"]
				entry_exit_time = agent_info["EntryExitTime"]
				
				for weekday, i in days_of_a_week.items():
					flow_index = 0

					entry_exit_time_weekday = [eetw for eetw in entry_exit_time if eetw["Days"] == weekday]

					if entry_exit_time_weekday == []:
						continue

					entry_exit_time_weekday = pd.DataFrame(entry_exit_time_weekday)
					
					if not "Rate" in agent_info["entry_type"][0]:
						entry_exit_time_weekday["EntryTime"] = entry_exit_time_weekday["EntryTime"].transform(lambda x: int(x.split(":")[0]) * steps_in_a_hour + int(x.split(":")[1]) * steps_in_a_minute)
						entry_exit_time_weekday = entry_exit_time_weekday.sort_values("EntryTime")

						if flow_index == 0:
							if weekday == init_week_day and start_step_time > entry_exit_time_weekday.loc[0, "EntryTime"]:
								print("ERROR: The start step time (" + start_step_time + ") is greater than the entrance time of agent '" + agent + "' (" + eetw.loc["EntryTime"] + ").")
								sys.exit(-1)

						for j, eetw in entry_exit_time_weekday.iterrows():
							flow_type = pd.DataFrame([df for _, df in deterministic_flow.iterrows() if df.loc["FlowID"] == eetw.loc["FlowID"]])

							env_hours_schedule[agent_type_idx][i][2 * j] = eetw.loc["EntryTime"]

							if j > 0:
								env_flow_distr[agent_type_idx][i][flow_index-1] = distributions["Deterministic"]
								env_flow_distr_firstparam[agent_type_idx][i][flow_index-1] = 0
								env_flow_distr_secondparam[agent_type_idx][i][flow_index-1] = 0
							
							for k, f in flow_type.iterrows():
								ft, fa = f.loc["Room"].strip().split("-")

								a, b = parse_distribution(f.loc["Time"], f.loc["Dist"])

								env_flow[agent_type_idx][i][flow_index] = MapEncoding.to_value(ft.upper())
								env_flow_area[agent_type_idx][i][flow_index] = areas[fa]["ID"]
								env_flow_distr[agent_type_idx][i][flow_index] = distributions[f.loc["Dist"]]
								env_flow_distr_firstparam[agent_type_idx][i][flow_index] = int(a) * steps_in_a_minute
								env_flow_distr_secondparam[agent_type_idx][i][flow_index] = int(b) * steps_in_a_minute
								env_activity_type[agent_type_idx][i][flow_index] = f.loc["Activity"]

								flow_index = flow_index + 1
					else:
						n = 0
						
						entry_exit_time_weekday["EntryTime"] = entry_exit_time_weekday["EntryTime"].transform(lambda x: int(x.split(":")[0]) * steps_in_a_hour + int(x.split(":")[1]) * steps_in_a_minute)
						entry_exit_time_weekday["ExitTime"] = entry_exit_time_weekday["ExitTime"].transform(lambda x: int(x.split(":")[0]) * steps_in_a_hour + int(x.split(":")[1]) * steps_in_a_minute)
						entry_exit_time_weekday = entry_exit_time_weekday.sort_values("EntryTime")

						for j, eetw in entry_exit_time_weekday.iterrows():
							env_hours_schedule[agent_type_idx][i][2 * j] = eetw.loc["EntryTime"]
							env_hours_schedule[agent_type_idx][i][2 * j + 1] = eetw.loc["ExitTime"]
							
							a, b = parse_distribution(eetw.loc["RateTime"], eetw.loc["RateDist"])
							env_birth_rates_distr[agent_type_idx][i][j] = distributions[eetw.loc["RateDist"]]
							env_birth_rates_distr_firstparam[agent_type_idx][i][j] = int(a)
							env_birth_rates_distr_secondparam[agent_type_idx][i][j] = int(b)

							if flow_index == 0:
								if weekday == init_week_day and start_step_time > entry_exit_time_weekday.loc[0, "EntryTime"]:
									print("ERROR: The start step time (" + start_step_time + ") is greater than the entrance time of agent '" + agent + "' (" + entry_exit_time_weekday.loc[0, "EntryTime"] + ").")
									sys.exit(-1)
						
						
						for k, f in deterministic_flow.iterrows():
							ft, fa = f.loc["Room"].strip().split("-")

							a, b = parse_distribution(f.loc["Time"], f.loc["Dist"])
								
							env_flow[agent_type_idx][i][flow_index] = MapEncoding.to_value(ft.upper())
							env_flow_area[agent_type_idx][i][flow_index] = areas[fa]["ID"]
							env_flow_distr[agent_type_idx][i][flow_index] = distributions[f.loc["Dist"]]
							env_flow_distr_firstparam[agent_type_idx][i][flow_index] = int(a) * steps_in_a_minute
							env_flow_distr_secondparam[agent_type_idx][i][flow_index] = int(b) * steps_in_a_minute
							env_activity_type[agent_type_idx][i][flow_index] = f.loc["Activity"]

							flow_index = flow_index + 1

					e = len(random_flow) - 1
					for rf in random_flow:
						if rf["Room"] == "Do nothing":
								continue

						room = rf["Room"].strip().split("-")

						a, b = parse_distribution(rf["Time"], rf["Dist"])

						env_events[agent_type_idx][e] = MapEncoding.to_value(room[0].upper())
						env_events_area[agent_type_idx][e] = areas[room[1]]["ID"]
						if e == len(random_flow):
							env_events_cdf[agent_type_idx][e] = float(rf["Weight"])
						else:
							env_events_cdf[agent_type_idx][e] = float(rf["Weight"]) + env_events_cdf[agent_type_idx][e+1]
					env_events_activity[agent_type_idx][e] = rf["Activity"]
					env_events_distr[agent_type_idx][e] = distributions[rf["Dist"]]
					env_events_distr_firstparam[agent_type_idx][e] = int(a) * steps_in_a_minute
					env_events_distr_secondparam[agent_type_idx][e] = int(b) * steps_in_a_minute

					e = e - 1
					
					env_events[agent_type_idx][0] = 0
					env_events_area[agent_type_idx][0] = -1
					env_events_activity[agent_type_idx][e] = 1.0
					env_events_cdf[agent_type_idx][0] = 1.0
					env_events_distr[agent_type_idx][0] = distributions["Deterministic"]
					env_events_distr_firstparam[agent_type_idx][0] = 0
					env_events_distr_secondparam[agent_type_idx][0] = 0

				for i in range(n):
					agent_file.write("\t<xagent>\n")
					agent_file.write("\t\t<name>pedestrian</name>\n")
					# agent_file.write("\t\t<cuda_initialized>0</cuda_initialized>\n")
					# agent_file.write("\t\t<x>" + str(starting_x) + "</x>\n")
					# agent_file.write("\t\t<y>" + str(starting_y) + "</y>\n")
					# agent_file.write("\t\t<z>" + str(starting_z) + "</z>\n")
					# agent_file.write("\t\t<animate>0</animate>\n")
					# agent_file.write("\t\t<velx>0.0</velx>\n")
					# agent_file.write("\t\t<vely>0.0</vely>\n")
					# agent_file.write("\t\t<velz>0.0</velz>\n")
					# agent_file.write("\t\t<quanta_inhaled>0.0</quanta_inhaled>\n")
					# agent_file.write("\t\t<final_target>" + str(starting_x) + "," + str(entrance_y_coords) + "," + str(starting_z) + "</final_target>\n")
					# agent_file.write("\t\t<intermediate_targets_x>" + ','.join(map(str, intermediate_targets_x_init)) + "</intermediate_targets_x>\n")
					# agent_file.write("\t\t<intermediate_targets_y>" + ','.join(map(str, intermediate_targets_y_init)) + "</intermediate_targets_y>\n")
					# agent_file.write("\t\t<intermediate_targets_z>" + ','.join(map(str, intermediate_targets_z_init)) + "</intermediate_targets_z>\n")
					# agent_file.write("\t\t<stay>" + ','.join(map(str, stay)) + "</stay>\n")
					# agent_file.write("\t\t<next_index>0</next_index>\n")
					# agent_file.write("\t\t<target_index>0</target_index>\n")
					# agent_file.write("\t\t<flow_index>" + str(days_of_a_week[init_week_day] * sol_length) + "</flow_index>\n")
					# agent_file.write("\t\t<incubation_days>0</incubation_days>\n")
					# agent_file.write("\t\t<infection_days>" + str(infection_days) + "</infection_days>\n")
					# agent_file.write("\t\t<fatality_days>" + str(fatality_days) + "</fatality_days>\n")
					# agent_file.write("\t\t<end_of_immunization_days>" + str(end_of_immunization_days) + "</end_of_immunization_days>\n")
					# agent_file.write("\t\t<init>0</init>\n")
					# agent_file.write("\t\t<infected_contacts_steps>0</infected_contacts_steps>\n")
					# agent_file.write("\t\t<animate_dir>1</animate_dir>\n")
					agent_file.write("\t\t<contacts_id>" + str(agents_count) + "</contacts_id>\n")
					# agent_file.write("\t\t<disease_state>" + str(disease_state) + "</disease_state>\n")
					# agent_file.write("\t\t<mask_type>" + str(env_mask_type[agent_type_idx] if random_mask < env_mask_fraction[agent_type_idx] else mask_types["No mask"]) + "</mask_type>\n")
					# agent_file.write("\t\t<room_for_quarantine_index>-1</room_for_quarantine_index>\n")
					agent_file.write("\t\t<agent_type>" + str(agent_type_idx) + "</agent_type>\n")
					# agent_file.write("\t\t<agent_with_a_rate>0</agent_with_a_rate>\n")
					# agent_file.write("\t\t<severity>0</severity>\n")
					# agent_file.write("\t\t<quarantine>0</quarantine>\n")
					# agent_file.write("\t\t<identified_infected>0</identified_infected>\n")
					# agent_file.write("\t\t<swab_steps>" + str(swab_steps) + "</swab_steps>\n")
					# agent_file.write("\t\t<entry_time_index>0</entry_time_index>\n")
					# agent_file.write("\t\t<just_exited_from_quarantine>0</just_exited_from_quarantine>\n")
					# agent_file.write("\t\t<week_day_flow>" + str(weekday_agent) + "</week_day_flow>\n")
					# agent_file.write("\t\t<in_an_event>0</in_an_event>\n")
					# agent_file.write("\t\t<last_step_move>0</last_step_move>\n")
					agent_file.write("\t</xagent>\n")

					agents_count = agents_count + 1

				if "Rate" in agent_info["entry_type"][0]:
					nawar = nawar + 1

				agent_type_idx = agent_type_idx + 1

			agent_file.write("</agents>\n")

		autogenerated_variables_names.write("#define ID \"id\"\n")
		autogenerated_variables_names.write("#define CUDA_INITIALIZED \"cuda_initialized\"\n")
		autogenerated_variables_names.write("#define X \"x\"\n")
		autogenerated_variables_names.write("#define Y \"y\"\n")
		autogenerated_variables_names.write("#define Z \"z\"\n")
		autogenerated_variables_names.write("#define ANIMATE \"animate\"\n")
		autogenerated_variables_names.write("#define VELX \"velx\"\n")
		autogenerated_variables_names.write("#define VELY \"vely\"\n")
		autogenerated_variables_names.write("#define VELZ \"velz\"\n")
		autogenerated_variables_names.write("#define QUANTA_INHALED \"quanta_inhaled\"\n")
		autogenerated_variables_names.write("#define FINAL_TARGET \"final_target\"\n")
		autogenerated_variables_names.write("#define INTERMEDIATE_TARGET_X \"intermediate_targets_x\"\n")
		autogenerated_variables_names.write("#define INTERMEDIATE_TARGET_Y \"intermediate_targets_y\"\n")
		autogenerated_variables_names.write("#define INTERMEDIATE_TARGET_Z \"intermediate_targets_z\"\n")
		autogenerated_variables_names.write("#define SOLUTION \"solution\"\n")
		autogenerated_variables_names.write("#define STAY \"stay\"\n")
		autogenerated_variables_names.write("#define NEXT_INDEX \"next_index\"\n")
		autogenerated_variables_names.write("#define TARGET_INDEX \"target_index\"\n")
		autogenerated_variables_names.write("#define FLOW_INDEX \"flow_index\"\n")
		autogenerated_variables_names.write("#define INCUBATION_DAYS \"incubation_days\"\n")
		autogenerated_variables_names.write("#define INFECTION_DAYS \"infection_days\"\n")
		autogenerated_variables_names.write("#define FATALITY_DAYS \"fatality_days\"\n")
		autogenerated_variables_names.write("#define END_OF_IMMUNIZATION_DAYS \"end_of_immunization_days\"\n")
		autogenerated_variables_names.write("#define INIT \"init\"\n")
		autogenerated_variables_names.write("#define INFECTED_CONTACTS_STEPS \"infected_contacts_steps\"\n")
		autogenerated_variables_names.write("#define ANIMATE_DIR \"animate_dir\"\n")
		autogenerated_variables_names.write("#define CONTACTS_ID \"contacts_id\"\n")
		autogenerated_variables_names.write("#define DISEASE_STATE \"disease_state\"\n")
		autogenerated_variables_names.write("#define MASK_TYPE \"mask_type\"\n")
		autogenerated_variables_names.write("#define ROOM_FOR_QUARANTINE_INDEX \"room_for_quarantine_index\"\n")
		autogenerated_variables_names.write("#define ACTIVITY_TYPE \"activity_type\"\n")
		autogenerated_variables_names.write("#define AGENT_TYPE \"agent_type\"\n")
		autogenerated_variables_names.write("#define AGENT_WITH_A_RATE \"agent_with_a_rate\"\n")
		autogenerated_variables_names.write("#define SEVERITY \"severity\"\n")
		autogenerated_variables_names.write("#define QUARANTINE \"quarantine\"\n")
		autogenerated_variables_names.write("#define IDENTIFIED_INFECTED \"identified_infected\"\n")
		autogenerated_variables_names.write("#define SWAB_STEPS \"swab_steps\"\n")
		autogenerated_variables_names.write("#define ENTRY_TIME_INDEX \"entry_time_index\"\n")
		autogenerated_variables_names.write("#define JUST_EXITED_FROM_QUARANTINE \"just_exited_from_quarantine\"\n\n")
		autogenerated_variables_names.write("#define WEEK_DAY_FLOW \"week_day_flow\"\n\n")
		autogenerated_variables_names.write("#define IN_AN_EVENT \"in_an_event\"\n\n")
		autogenerated_variables_names.write("#define LAST_STEP_MOVE \"last_step_move\"\n\n")
		autogenerated_variables_names.write("#define WAITING_ROOM_TIME \"waiting_room_time\"\n\n")
		autogenerated_variables_names.write("#define ENTRY_EXIT_FLAG \"entry_exit_flag\"\n\n")
		autogenerated_variables_names.write("#define WAITING_ROOM_FLAG \"waiting_room_flag\"\n\n")
		autogenerated_variables_names.write("#define NODE_WAITING_FOR \"node_waiting_for\"\n\n")

		counters = np.zeros(num_counters, dtype=int)

		cuda_rng_offsets_pedestrian = np.zeros(total_agents_overestimation, dtype=int)
		cuda_rng_offsets_room = np.zeros(len(WHOLEmodel["roomsINcanvas"]), dtype=int)

		#setting  resources
		for v in vlist:
			if len(v.resources) != 0:
				global_resources[v.id] = v.resources.loc["MAX", 0]
				
		for v in vlist:
			if len(v.resources) != 0:
				for agent, ID in pedestrian_names.items():
					if agent in v.resources.index:
						specific_resources[ID][v.id] = v.resources.loc[agent, 0]

		for v in vlist:
			if len(v.waitingroom) != 0:
				for agent, ID in pedestrian_names.items():
					if agent in v.waitingroom.index:
						key = v.waitingroom.loc[agent, "Room"]
						if key == "Same room":
							alternative_resources_area[ID][v.id] = v.area
							alternative_resources_type[ID][v.id] = v.type.value
						elif key == "Skip room":
							alternative_resources_area[ID][v.id] = -1
							alternative_resources_type[ID][v.id] = -1
						else:
							type_area = (key.replace(" ", "")).split("-")
							alternative_resources_type[ID][v.id] = MapEncoding.to_value(type_area[0].upper())
							alternative_resources_area[ID][v.id] = areas[type_area[1]]["ID"]
		
		macro_environment_dir = "macro_environment/"
		os.system("mkdir -p " + macro_environment_dir)

		with open(macro_environment_dir + "COORD2INDEX.xml", "w") as file:
			file.write("<states><macro_environment><COORD2INDEX>")
			for k in range(num_floors):
				for i in range(env_dims[2]):
					for j in range(env_dims[0]):
						file.write(str(coord2index[k][i][j]) + ("" if((i == env_dims[2] - 1) and (j == env_dims[0] - 1) and (k == num_floors - 1)) else ","))
			file.write("</COORD2INDEX></macro_environment></states>\n")
		autogenerated_variables_names.write("#define COORD2INDEX \"COORD2INDEX\"\n")

		with open(macro_environment_dir + "ADJMATRIX.xml", "w") as file:
			file.write("<states><macro_environment><ADJMATRIX>")
			elist = sorted(final_graph.edgelist, key = lambda e: (e.v1.id, e.v2.id))
			for e in elist:
				adjmatrix[e.v1.id][e.v2.id] = int(e.w)
				adjmatrix[e.v2.id][e.v1.id] = int(e.w)

			for j in range(len(vlist)):
				for i in range(len(vlist)):
					file.write(str(int(adjmatrix[j][i])) + ("" if((i == len(vlist) - 1) and (j == len(vlist) - 1)) else ",")) 
			file.write("</ADJMATRIX></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ADJMATRIX \"ADJMATRIX\"\n")

		with open(macro_environment_dir + "ENV_FLOW.xml", "w") as file:
			file.write("<states><macro_environment><ENV_FLOW>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_flow[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_FLOW></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_FLOW \"ENV_FLOW\"\n")

		with open(macro_environment_dir + "ENV_FLOW_AREA.xml", "w") as file:
			file.write("<states><macro_environment><ENV_FLOW_AREA>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_flow_area[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_FLOW_AREA></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_FLOW_AREA \"ENV_FLOW_AREA\"\n")

		with open(macro_environment_dir + "ENV_FLOW_DISTR.xml", "w") as file:
			file.write("<states><macro_environment><ENV_FLOW_DISTR>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_flow_distr[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_FLOW_DISTR></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_FLOW_DISTR \"ENV_FLOW_DISTR\"\n")

		with open(macro_environment_dir + "ENV_FLOW_DISTR_FIRSTPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_FLOW_DISTR_FIRSTPARAM>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_flow_distr_firstparam[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_FLOW_DISTR_FIRSTPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_FLOW_DISTR_FIRSTPARAM \"ENV_FLOW_DISTR_FIRSTPARAM\"\n")

		with open(macro_environment_dir + "ENV_FLOW_DISTR_SECONDPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_FLOW_DISTR_SECONDPARAM>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_flow_distr_secondparam[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_FLOW_DISTR_SECONDPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_FLOW_DISTR_SECONDPARAM \"ENV_FLOW_DISTR_SECONDPARAM\"\n")

		with open(macro_environment_dir + "ENV_ACTIVITY_TYPE.xml", "w") as file:
			file.write("<states><macro_environment><ENV_ACTIVITY_TYPE>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_activity_type[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_ACTIVITY_TYPE></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_ACTIVITY_TYPE \"ENV_ACTIVITY_TYPE\"\n")

		with open(macro_environment_dir + "ENV_HOURS_SCHEDULE.xml", "w") as file:
			file.write("<states><macro_environment><ENV_HOURS_SCHEDULE>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_hours_schedule[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_HOURS_SCHEDULE></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_HOURS_SCHEDULE \"ENV_HOURS_SCHEDULE\"\n")

		with open(macro_environment_dir + "ENV_BIRTH_RATE_DISTR.xml", "w") as file:
			file.write("<states><macro_environment><ENV_BIRTH_RATE_DISTR>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_birth_rates_distr[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_BIRTH_RATE_DISTR></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_BIRTH_RATE_DISTR \"ENV_BIRTH_RATE_DISTR\"\n")

		with open(macro_environment_dir + "ENV_BIRTH_RATE_DISTR_FIRSTPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_BIRTH_RATE_DISTR_FIRSTPARAM>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_birth_rates_distr_firstparam[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_BIRTH_RATE_DISTR_FIRSTPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_BIRTH_RATE_DISTR_FIRSTPARAM \"ENV_BIRTH_RATE_DISTR_FIRSTPARAM\"\n")

		with open(macro_environment_dir + "ENV_BIRTH_RATE_DISTR_SECONDPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_BIRTH_RATE_DISTR_SECONDPARAM>")
			for k in range(total_number_of_agents_types):
				for i in range(7):
					for j in range(sol_length):
						file.write(str(env_birth_rates_distr_secondparam[k][i][j]) + ("" if((i == 6) and (j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_BIRTH_RATE_DISTR_SECONDPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_BIRTH_RATE_DISTR_SECONDPARAM \"ENV_BIRTH_RATE_DISTR_SECONDPARAM\"\n")

		with open(macro_environment_dir + "ENV_EVENTS.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EVENTS>")
			for k in range(total_number_of_agents_types):
				for j in range(sol_length):
					file.write(str(env_events[k][j]) + ("" if((j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_EVENTS></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EVENTS \"ENV_EVENTS\"\n")

		with open(macro_environment_dir + "ENV_EVENTS_AREA.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EVENTS_AREA>")
			for k in range(total_number_of_agents_types):
				for j in range(sol_length):
					file.write(str(env_events_area[k][j]) + ("" if((j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_EVENTS_AREA></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EVENTS_AREA \"ENV_EVENTS_AREA\"\n")

		with open(macro_environment_dir + "ENV_EVENTS_ACTIVITY_TYPE.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EVENTS_ACTIVITY_TYPE>")
			for k in range(total_number_of_agents_types):
				for j in range(sol_length):
					file.write(str(env_events_activity[k][j]) + ("" if((j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_EVENTS_ACTIVITY_TYPE></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EVENTS_ACTIVITY_TYPE \"ENV_EVENTS_ACTIVITY_TYPE\"\n")

		with open(macro_environment_dir + "ENV_EVENTS_CDF.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EVENTS_CDF>")
			for k in range(total_number_of_agents_types):
				for j in range(sol_length):
					file.write(str(env_events_cdf[k][j]) + ("" if((j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_EVENTS_CDF></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EVENTS_CDF \"ENV_EVENTS_CDF\"\n")

		with open(macro_environment_dir + "ENV_EVENTS_DISTR.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EVENTS_DISTR>")
			for k in range(total_number_of_agents_types):
				for j in range(sol_length):
					file.write(str(env_events_distr[k][j]) + ("" if((j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_EVENTS_DISTR></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EVENTS_DISTR \"ENV_EVENTS_DISTR\"\n")

		with open(macro_environment_dir + "ENV_EVENTS_DISTR_FIRSTPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EVENTS_DISTR_FIRSTPARAM>")
			for k in range(total_number_of_agents_types):
				for j in range(sol_length):
					file.write(str(env_events_distr_firstparam[k][j]) + ("" if((j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_EVENTS_DISTR_FIRSTPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EVENTS_DISTR_FIRSTPARAM \"ENV_EVENTS_DISTR_FIRSTPARAM\"\n")

		with open(macro_environment_dir + "ENV_EVENTS_DISTR_SECONDPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EVENTS_DISTR_SECONDPARAM>")
			for k in range(total_number_of_agents_types):
				for j in range(sol_length):
					file.write(str(env_events_distr_secondparam[k][j]) + ("" if((j == sol_length - 1) and (k == total_number_of_agents_types - 1)) else ","))
			file.write("</ENV_EVENTS_DISTR_SECONDPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EVENTS_DISTR_SECONDPARAM \"ENV_EVENTS_DISTR_SECONDPARAM\"\n")

		with open(macro_environment_dir + "ENV_MASK_TYPE.xml", "w") as file:
			file.write("<states><macro_environment><ENV_MASK_TYPE>" + ','.join(map(str, env_mask_type)) + "</ENV_MASK_TYPE></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_MASK_TYPE \"ENV_MASK_TYPE\"\n")

		with open(macro_environment_dir + "ENV_MASK_FRACTION.xml", "w") as file:
			file.write("<states><macro_environment><ENV_MASK_FRACTION>" + ','.join(map(str, env_mask_fraction)) + "</ENV_MASK_FRACTION></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_MASK_FRACTION \"ENV_MASK_FRACTION\"\n")

		with open(macro_environment_dir + "ENV_VACCINATION_FRACTION.xml", "w") as file:
			file.write("<states><macro_environment><ENV_VACCINATION_FRACTION>" + ','.join(map(str, env_vaccination_fraction)) + "</ENV_VACCINATION_FRACTION></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_VACCINATION_FRACTION \"ENV_VACCINATION_FRACTION\"\n")

		with open(macro_environment_dir + "ENV_VACCINATION_EFFICACY.xml", "w") as file:
			file.write("<states><macro_environment><ENV_VACCINATION_EFFICACY>" + ','.join(map(str, env_vaccination_efficacy)) + "</ENV_VACCINATION_EFFICACY></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_VACCINATION_EFFICACY \"ENV_VACCINATION_EFFICACY\"\n")

		with open(macro_environment_dir + "ENV_SWAB_DISTR.xml", "w") as file:
			file.write("<states><macro_environment><ENV_SWAB_DISTR>" + ','.join(map(str, env_swab_distr)) + "</ENV_SWAB_DISTR></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_SWAB_DISTR \"ENV_SWAB_DISTR\"\n")

		with open(macro_environment_dir + "ENV_SWAB_DISTR_FIRSTPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_SWAB_DISTR_FIRSTPARAM>" + ','.join(map(str, env_swab_distr_firstparam)) + "</ENV_SWAB_DISTR_FIRSTPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_SWAB_DISTR_FIRSTPARAM \"ENV_SWAB_DISTR_FIRSTPARAM\"\n")

		with open(macro_environment_dir + "ENV_SWAB_DISTR_SECONDPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_SWAB_DISTR_SECONDPARAM>" + ','.join(map(str, env_swab_distr_secondparam)) + "</ENV_SWAB_DISTR_SECONDPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_SWAB_DISTR_SECONDPARAM \"ENV_SWAB_DISTR_SECONDPARAM\"\n")

		with open(macro_environment_dir + "ENV_QUARANTINE_DAYS_DISTR.xml", "w") as file:
			file.write("<states><macro_environment><ENV_QUARANTINE_DAYS_DISTR>" + ','.join(map(str, env_quarantine_days_distr)) + "</ENV_QUARANTINE_DAYS_DISTR></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_QUARANTINE_DAYS_DISTR \"ENV_QUARANTINE_DAYS_DISTR\"\n")

		with open(macro_environment_dir + "ENV_QUARANTINE_DAYS_DISTR_FIRSTPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_QUARANTINE_DAYS_DISTR_FIRSTPARAM>" + ','.join(map(str, env_quarantine_days_distr_firstparam)) + "</ENV_QUARANTINE_DAYS_DISTR_FIRSTPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_QUARANTINE_DAYS_DISTR_FIRSTPARAM \"ENV_QUARANTINE_DAYS_DISTR_FIRSTPARAM\"\n")

		with open(macro_environment_dir + "ENV_QUARANTINE_DAYS_DISTR_SECONDPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_QUARANTINE_DAYS_DISTR_SECONDPARAM>" + ','.join(map(str, env_quarantine_days_distr_secondparam)) + "</ENV_QUARANTINE_DAYS_DISTR_SECONDPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_QUARANTINE_DAYS_DISTR_SECONDPARAM \"ENV_QUARANTINE_DAYS_DISTR_SECONDPARAM\"\n")

		with open(macro_environment_dir + "ENV_QUARANTINE_SWAB_DAYS_DISTR.xml", "w") as file:
			file.write("<states><macro_environment><ENV_QUARANTINE_SWAB_DAYS_DISTR>" + ','.join(map(str, env_quarantine_swab_days_distr)) + "</ENV_QUARANTINE_SWAB_DAYS_DISTR></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_QUARANTINE_SWAB_DAYS_DISTR \"ENV_QUARANTINE_SWAB_DAYS_DISTR\"\n")

		with open(macro_environment_dir + "ENV_QUARANTINE_SWAB_DAYS_DISTR_FIRSTPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_QUARANTINE_SWAB_DAYS_DISTR_FIRSTPARAM>" + ','.join(map(str, env_quarantine_swab_days_distr_firstparam)) + "</ENV_QUARANTINE_SWAB_DAYS_DISTR_FIRSTPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_QUARANTINE_SWAB_DAYS_DISTR_FIRSTPARAM \"ENV_QUARANTINE_SWAB_DAYS_DISTR_FIRSTPARAM\"\n")

		with open(macro_environment_dir + "ENV_QUARANTINE_SWAB_DAYS_DISTR_SECONDPARAM.xml", "w") as file:
			file.write("<states><macro_environment><ENV_QUARANTINE_SWAB_DAYS_DISTR_SECONDPARAM>" + ','.join(map(str, env_quarantine_swab_days_distr_secondparam)) + "</ENV_QUARANTINE_SWAB_DAYS_DISTR_SECONDPARAM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_QUARANTINE_SWAB_DAYS_DISTR_SECONDPARAM \"ENV_QUARANTINE_SWAB_DAYS_DISTR_SECONDPARAM\"\n")

		with open(macro_environment_dir + "ENV_ROOM_FOR_QUARANTINE_TYPE.xml", "w") as file:
			file.write("<states><macro_environment><ENV_ROOM_FOR_QUARANTINE_TYPE>" + ','.join(map(str, env_room_for_quarantine_type)) + "</ENV_ROOM_FOR_QUARANTINE_TYPE></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_ROOM_FOR_QUARANTINE_TYPE \"ENV_ROOM_FOR_QUARANTINE_TYPE\"\n")

		with open(macro_environment_dir + "ENV_ROOM_FOR_QUARANTINE_AREA.xml", "w") as file:
			file.write("<states><macro_environment><ENV_ROOM_FOR_QUARANTINE_AREA>" + ','.join(map(str, env_room_for_quarantine_area)) + "</ENV_ROOM_FOR_QUARANTINE_AREA></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_ROOM_FOR_QUARANTINE_AREA \"ENV_ROOM_FOR_QUARANTINE_AREA\"\n")

		with open(macro_environment_dir + "ENV_EXTERNAL_SCREENING_FIRST.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EXTERNAL_SCREENING_FIRST>" + ','.join(map(str, env_external_screening_first)) + "</ENV_EXTERNAL_SCREENING_FIRST></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EXTERNAL_SCREENING_FIRST \"ENV_EXTERNAL_SCREENING_FIRST\"\n")

		with open(macro_environment_dir + "ENV_EXTERNAL_SCREENING_SECOND.xml", "w") as file:
			file.write("<states><macro_environment><ENV_EXTERNAL_SCREENING_SECOND>" + ','.join(map(str, env_external_screening_second)) + "</ENV_EXTERNAL_SCREENING_SECOND></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ENV_EXTERNAL_SCREENING_SECOND \"ENV_EXTERNAL_SCREENING_SECOND\"\n")

		with open(macro_environment_dir + "INITIAL_INFECTED.xml", "w") as file:
			file.write("<states><macro_environment><INITIAL_INFECTED>" + ','.join(map(str, initial_infected_agents)) + "</INITIAL_INFECTED></macro_environment></states>\n")
		autogenerated_variables_names.write("#define INITIAL_INFECTED \"INITIAL_INFECTED\"\n")

		with open(macro_environment_dir + "NUMBER_OF_AGENTS_BY_TYPE.xml", "w") as file:
			file.write("<states><macro_environment><NUMBER_OF_AGENTS_BY_TYPE>" + ','.join(map(str, number_of_agents_by_type)) + "</NUMBER_OF_AGENTS_BY_TYPE></macro_environment></states>\n")
		autogenerated_variables_names.write("#define NUMBER_OF_AGENTS_BY_TYPE \"NUMBER_OF_AGENTS_BY_TYPE\"\n")

		autogenerated_variables_names.write("#define NUMBER_OF_STEPS_CONTACTS \"NUMBER_OF_STEPS_CONTACTS\"\n")

		with open(macro_environment_dir + "COMPARTMENTAL_MODEL.xml", "w") as file:
			file.write("<states><macro_environment><COMPARTMENTAL_MODEL>" + ','.join(map(str, compartmental_model)) + "</COMPARTMENTAL_MODEL></macro_environment></states>\n")
		autogenerated_variables_names.write("#define COMPARTMENTAL_MODEL \"COMPARTMENTAL_MODEL\"\n")

		with open(macro_environment_dir + "ROOMS_QUANTA_CONCENTRATION.xml", "w") as file:
			file.write("<states><macro_environment><ROOMS_QUANTA_CONCENTRATION>" + ','.join(map(str, rooms_quanta_concentration)) + "</ROOMS_QUANTA_CONCENTRATION></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ROOMS_QUANTA_CONCENTRATION \"ROOMS_QUANTA_CONCENTRATION\"\n\n")

		with open(macro_environment_dir + "GLOBAL_RESOURCES.xml", "w") as file:
			file.write("<states><macro_environment><GLOBAL_RESOURCES>" + ','.join(map(str, global_resources)) + "</GLOBAL_RESOURCES></macro_environment></states>\n")
		autogenerated_variables_names.write("#define GLOBAL_RESOURCES \"GLOBAL_RESOURCES\"\n\n")
		
		with open(macro_environment_dir + "GLOBAL_RESOURCES_COUNTER.xml", "w") as file:
			file.write("<states><macro_environment><GLOBAL_RESOURCES_COUNTER>" + ','.join(map(str, global_resources_counter)) + "</GLOBAL_RESOURCES_COUNTER></macro_environment></states>\n")
		autogenerated_variables_names.write("#define GLOBAL_RESOURCES_COUNTER \"GLOBAL_RESOURCES_COUNTER\"\n\n")

		with open(macro_environment_dir + "SPECIFIC_RESOURCES.xml", "w") as file:
			file.write("<states><macro_environment><SPECIFIC_RESOURCES>")
			for k in range(total_number_of_agents_types):
				for j in range(len(vlist)):
					file.write(str(specific_resources[k][j]) + ("" if((k == total_number_of_agents_types - 1) and (j == len(vlist) - 1)) else ","))
			file.write("</SPECIFIC_RESOURCES></macro_environment></states>\n")
		autogenerated_variables_names.write("#define SPECIFIC_RESOURCES \"SPECIFIC_RESOURCES\"\n\n")

		with open(macro_environment_dir + "SPECIFIC_RESOURCES_COUNTER.xml", "w") as file:
			file.write("<states><macro_environment><SPECIFIC_RESOURCES_COUNTER>")
			for k in range(total_number_of_agents_types):
				for j in range(len(vlist)):
					file.write(str(specific_resources_counter[k][j]) + ("" if((k == total_number_of_agents_types - 1) and (j == len(vlist) - 1)) else ","))
			file.write("</SPECIFIC_RESOURCES_COUNTER></macro_environment></states>\n")
		autogenerated_variables_names.write("#define SPECIFIC_RESOURCES_COUNTER \"SPECIFIC_RESOURCES_COUNTER\"\n")

		with open(macro_environment_dir + "ALTERNATIVE_RESOURCES_TYPE.xml", "w") as file:
			file.write("<states><macro_environment><ALTERNATIVE_RESOURCES_TYPE>")
			for k in range(total_number_of_agents_types):
				for j in range(len(vlist)):
					file.write(str(alternative_resources_type[k][j]) + ("" if((k == total_number_of_agents_types - 1) and (j == len(vlist) - 1)) else ","))
			file.write("</ALTERNATIVE_RESOURCES_TYPE></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ALTERNATIVE_RESOURCES_TYPE \"ALTERNATIVE_RESOURCES_TYPE\"\n")

		with open(macro_environment_dir + "ALTERNATIVE_RESOURCES_AREA.xml", "w") as file:
			file.write("<states><macro_environment><ALTERNATIVE_RESOURCES_AREA>")
			for k in range(total_number_of_agents_types):
				for j in range(len(vlist)):
					file.write(str(alternative_resources_area[k][j]) + ("" if((k == total_number_of_agents_types - 1) and (j == len(vlist) - 1)) else ","))
			file.write("</ALTERNATIVE_RESOURCES_AREA></macro_environment></states>\n")
		autogenerated_variables_names.write("#define ALTERNATIVE_RESOURCES_AREA \"ALTERNATIVE_RESOURCES_AREA\"\n")

		with open(macro_environment_dir + "COUNTERS.xml", "w") as file:
			file.write("<states><macro_environment><COUNTERS>" + ','.join(map(str, counters)) + "</COUNTERS></macro_environment></states>\n")
		autogenerated_variables_names.write("#define COUNTERS \"COUNTERS\"\n")

		with open(macro_environment_dir + "CUDA_RNG_OFFSETS_PEDESTRIAN.xml", "w") as file:
			file.write("<states><macro_environment><CUDA_RNG_OFFSETS_PEDESTRIAN>" + ','.join(map(str, cuda_rng_offsets_pedestrian)) + "</CUDA_RNG_OFFSETS_PEDESTRIAN></macro_environment></states>\n")
		autogenerated_variables_names.write("#define CUDA_RNG_OFFSETS_PEDESTRIAN \"CUDA_RNG_OFFSETS_PEDESTRIAN\"\n")

		with open(macro_environment_dir + "CUDA_RNG_OFFSETS_ROOM.xml", "w") as file:
			file.write("<states><macro_environment><CUDA_RNG_OFFSETS_ROOM>" + ','.join(map(str, cuda_rng_offsets_room)) + "</CUDA_RNG_OFFSETS_ROOM></macro_environment></states>\n")
		autogenerated_variables_names.write("#define CUDA_RNG_OFFSETS_ROOM \"CUDA_RNG_OFFSETS_ROOM\"\n")

		contamination_risk = disease["beta_contact"][0]
		contamination_risk_decreased_with_masks = 0.47
		ngen_base = 0.589
		vl = 9
		virus_variant_factor = float(WHOLEmodel["virus_variant"][0])
		decay_rate = 0.636 / 3600
		gravitational_settling_rate = 0.39 / 3600
		exhalation_efficacy_no_surgical_ffp2_masks = [0, 0.59, 0.9]
		inhalation_efficacy_no_surgical_ffp2_masks = [0, 0.59, 0.9]
		inhalation_rate_pure = 0.521
		risk_const = disease["beta_aerosol"][0]

		sensitivity_swab = float(whatif["swab_sensitivity"][0])
		specificity_swab = float(whatif["swab_specificity"][0])
		virus_severity = float(WHOLEmodel["virus_severity"][0])
		
		nrun = int(WHOLEmodel["starting"][0]["nrun"])
		if ensemble == "ON":
			configuration_file.write("{\n")
			configuration_file.write("\t\"RunPlanVector\": [\n")
			for run in range(nrun):	
				os.system("mkdir -p ../results/" + dirname_experiment + "/seed" + str(random_seed + run))

				configuration_file.write("\t\t{\n")
				configuration_file.write("\t\t\t\"random_seed\": " + str(random_seed + run) + ",\n")
				configuration_file.write("\t\t\t\"steps\": " + str(str(steps_in_a_day * days)) + ",\n")
				configuration_file.write("\t\t\t\"output_subdirectory\": \"results/" + dirname_experiment + "/seed" + str(random_seed + run) + "/\",\n")
				configuration_file.write("\t\t\t\"properties\": {\n")

				configuration_file.write("\t\t\t\t\"SEED\": " + str(random_seed + run) + ",\n")
				configuration_file.write("\t\t\t\t\"INDEX2COORDX\": [" + ','.join(map(str, index2coord[0])) + "],\n")
				configuration_file.write("\t\t\t\t\"INDEX2COORDY\": [" + ','.join(map(str, index2coord[1])) + "],\n")
				configuration_file.write("\t\t\t\t\"INDEX2COORDZ\": [" + ','.join(map(str, index2coord[2])) + "],\n")
				configuration_file.write("\t\t\t\t\"NODE_TYPE\": [" + ','.join(map(str, node_type)) + "],\n")
				configuration_file.write("\t\t\t\t\"NODE_YAW\": [" + ','.join(map(str, node_yaw)) + "],\n")
				configuration_file.write("\t\t\t\t\"NODE_LENGTH\": [" + ','.join(map(str, node_length)) + "],\n")
				configuration_file.write("\t\t\t\t\"NODE_WIDTH\": [" + ','.join(map(str, node_width)) + "],\n")
				configuration_file.write("\t\t\t\t\"EXTERN_RANGES\": [" + ','.join(map(str, starting_x_range)) + "," + ','.join(map(str, starting_z_range)) + "],\n")
				configuration_file.write("\t\t\t\t\"EXTERN_NODE\": " + str(extern_node) + ",\n")
				configuration_file.write("\t\t\t\t\"NEXT_CONTACTS_ID\": " + str(agents_count) + ",\n")
				configuration_file.write("\t\t\t\t\"DAY\": 1,\n")
				configuration_file.write("\t\t\t\t\"WEEK_DAY\": " + str(days_of_a_week[init_week_day]) + ",\n")
				configuration_file.write("\t\t\t\t\"MEAN_INCUBATION_DAYS\": [" + str(mean_incubation_days_dist) + "," + str(mean_incubation_days_time[0]) + "," + str(mean_incubation_days_time[1]) + "],\n")
				configuration_file.write("\t\t\t\t\"MEAN_INFECTION_DAYS\": [" + str(mean_infection_days_dist) + "," + str(mean_infection_days_time[0]) + "," + str(mean_infection_days_time[1]) + "],\n")
				configuration_file.write("\t\t\t\t\"MEAN_END_OF_IMMUNIZATION_DAYS\": [" + str(mean_end_of_immunization_days_dist) + "," + str(mean_end_of_immunization_days_time[0]) + "," + str(mean_end_of_immunization_days_time[1]) + "],\n")
				configuration_file.write("\t\t\t\t\"MEAN_FATALITY_DAYS\": [" + str(mean_fatality_days_dist) + "," + str(mean_fatality_days_time[0]) + "," + str(mean_fatality_days_time[1]) + "],\n")
				configuration_file.write("\t\t\t\t\"EXHALATION_MASK_EFFICACY\": [" + ','.join(map(str, exhalation_efficacy_no_surgical_ffp2_masks)) + "],\n")
				configuration_file.write("\t\t\t\t\"INHALATION_MASK_EFFICACY\": [" + ','.join(map(str, inhalation_efficacy_no_surgical_ffp2_masks)) + "],\n")
				configuration_file.write("\t\t\t\t\"CONTAMINATION_RISK\": " + str(contamination_risk) + ",\n")
				configuration_file.write("\t\t\t\t\"CONTAMINATION_RISK_DECREASED_WITH_MASK\": " + str(contamination_risk_decreased_with_masks) + ",\n")
				configuration_file.write("\t\t\t\t\"NGEN_BASE\": " + str(ngen_base) + ",\n")
				configuration_file.write("\t\t\t\t\"VL\": " + str(vl) + ",\n")
				configuration_file.write("\t\t\t\t\"VIRUS_VARIANT_FACTOR\": " + str(virus_variant_factor) + ",\n")
				configuration_file.write("\t\t\t\t\"DECAY_RATE\": " + str(decay_rate) + ",\n")
				configuration_file.write("\t\t\t\t\"GRAVITATIONAL_SETTLING_RATE\": " + str(gravitational_settling_rate) + ",\n")
				configuration_file.write("\t\t\t\t\"INHALATION_RATE_PURE\": " + str(inhalation_rate_pure) + ",\n")
				configuration_file.write("\t\t\t\t\"RISK_CONST\": " + str(risk_const) + ",\n")
				configuration_file.write("\t\t\t\t\"PERC_INF\": [" + ','.join(map(str, perc_inf_df.loc[:, "percentage_infected"])) + "],\n")
				configuration_file.write("\t\t\t\t\"SENSITIVITY_SWAB\": " + str(sensitivity_swab) + ",\n")
				configuration_file.write("\t\t\t\t\"SPECIFICITY_SWAB\": " + str(specificity_swab) + ",\n")
				configuration_file.write("\t\t\t\t\"VIRUS_SEVERITY\": " + str(virus_severity) + ",\n")
				configuration_file.write("\t\t\t\t\"RUN_IDX\": " + str(run) + "\n")
				configuration_file.write("\t\t\t}\n")
				configuration_file.write("\t\t}" + ("\n" if run == nrun - 1 else ",\n"))
			configuration_file.write("\t]\n")
			configuration_file.write("}")
		else:
			os.system("mkdir -p ../results/" + dirname_experiment + "/seed" + str(random_seed))

			configuration_file.write("<states>\n")
			configuration_file.write("\t<config>\n")
			configuration_file.write("\t\t<simulation>\n")
			configuration_file.write("\t\t\t<exit_log_file>" + ("end.xml" if checkpoint == "ON" else "") + "</exit_log_file>\n")
			configuration_file.write("\t\t\t<random_seed>" + str(random_seed) + "</random_seed>\n")
			configuration_file.write("\t\t\t<steps>" + str(steps_in_a_day * days) + "</steps>\n")
			configuration_file.write("\t\t</simulation>\n")
			configuration_file.write("\t</config>\n")

			configuration_file.write("\t<environment>\n")
			configuration_file.write("\t\t<SEED>" + str(random_seed) + "</SEED>\n")
			configuration_file.write("\t\t<INDEX2COORDX>" + ','.join(map(str, index2coord[0])) + "</INDEX2COORDX>\n")
			configuration_file.write("\t\t<INDEX2COORDY>" + ','.join(map(str, index2coord[1])) + "</INDEX2COORDY>\n")
			configuration_file.write("\t\t<INDEX2COORDZ>" + ','.join(map(str, index2coord[2])) + "</INDEX2COORDZ>\n")
			configuration_file.write("\t\t<NODE_TYPE>" + ','.join(map(str, node_type)) + "</NODE_TYPE>\n")
			configuration_file.write("\t\t<NODE_YAW>" + ','.join(map(str, node_yaw)) + "</NODE_YAW>\n")
			configuration_file.write("\t\t<NODE_LENGTH>" + ','.join(map(str, node_length)) + "</NODE_LENGTH>\n")
			configuration_file.write("\t\t<NODE_WIDTH>" + ','.join(map(str, node_width)) + "</NODE_WIDTH>\n")
			configuration_file.write("\t\t<EXTERN_RANGES>" + ','.join(map(str, starting_x_range)) + "," + ','.join(map(str, starting_z_range)) + "</EXTERN_RANGES>\n")
			configuration_file.write("\t\t<EXTERN_NODE>" + str(extern_node) + "</EXTERN_NODE>\n")
			configuration_file.write("\t\t<NEXT_CONTACTS_ID>" + str(agents_count) + "</NEXT_CONTACTS_ID>\n")
			configuration_file.write("\t\t<DAY>1</DAY>\n")
			configuration_file.write("\t\t<WEEK_DAY>" + str(days_of_a_week[init_week_day]) + "</WEEK_DAY>\n")
			configuration_file.write("\t\t<MEAN_INCUBATION_DAYS>" + str(mean_incubation_days_dist) + "," + str(mean_incubation_days_time[0]) + "," + str(mean_incubation_days_time[1]) + "</MEAN_INCUBATION_DAYS>\n")
			configuration_file.write("\t\t<MEAN_INFECTION_DAYS>" + str(mean_infection_days_dist) + "," + str(mean_infection_days_time[0]) + "," + str(mean_infection_days_time[1]) + "</MEAN_INFECTION_DAYS>\n")
			configuration_file.write("\t\t<MEAN_END_OF_IMMUNIZATION_DAYS>" + str(mean_end_of_immunization_days_dist) + "," + str(mean_end_of_immunization_days_time[0]) + "," + str(mean_end_of_immunization_days_time[1]) + "</MEAN_END_OF_IMMUNIZATION_DAYS>\n")
			configuration_file.write("\t\t<MEAN_FATALITY_DAYS>" + str(mean_fatality_days_dist) + "," + str(mean_fatality_days_time[0]) + "," + str(mean_fatality_days_time[1]) + "</MEAN_FATALITY_DAYS>\n")
			configuration_file.write("\t\t<EXHALATION_MASK_EFFICACY>" + ','.join(map(str, exhalation_efficacy_no_surgical_ffp2_masks)) + "</EXHALATION_MASK_EFFICACY>\n")
			configuration_file.write("\t\t<INHALATION_MASK_EFFICACY>" + ','.join(map(str, inhalation_efficacy_no_surgical_ffp2_masks)) + "</INHALATION_MASK_EFFICACY>\n")
			configuration_file.write("\t\t<CONTAMINATION_RISK>" + str(contamination_risk) + "</CONTAMINATION_RISK>\n")
			configuration_file.write("\t\t<CONTAMINATION_RISK_DECREASED_WITH_MASK>" + str(contamination_risk_decreased_with_masks) + "</CONTAMINATION_RISK_DECREASED_WITH_MASK>\n")
			configuration_file.write("\t\t<NGEN_BASE>" + str(ngen_base) + "</NGEN_BASE>\n")
			configuration_file.write("\t\t<VL>" + str(vl) + "</VL>\n")
			configuration_file.write("\t\t<VIRUS_VARIANT_FACTOR>" + str(virus_variant_factor) + "</VIRUS_VARIANT_FACTOR>\n")
			configuration_file.write("\t\t<DECAY_RATE>" + str(decay_rate) + "</DECAY_RATE>\n")
			configuration_file.write("\t\t<GRAVITATIONAL_SETTLING_RATE>" + str(gravitational_settling_rate) + "</GRAVITATIONAL_SETTLING_RATE>\n")
			configuration_file.write("\t\t<INHALATION_RATE_PURE>" + str(inhalation_rate_pure) + "</INHALATION_RATE_PURE>\n")
			configuration_file.write("\t\t<RISK_CONST>" + str(risk_const) + "</RISK_CONST>\n")
			configuration_file.write("\t\t<PERC_INF>" + ','.join(map(str, perc_inf_df.loc[:, "percentage_infected"])) + "</PERC_INF>\n")
			configuration_file.write("\t\t<SENSITIVITY_SWAB>" + str(sensitivity_swab) + "</SENSITIVITY_SWAB>\n")
			configuration_file.write("\t\t<SPECIFICITY_SWAB>" + str(specificity_swab) + "</SPECIFICITY_SWAB>\n")
			configuration_file.write("\t\t<VIRUS_SEVERITY>" + str(virus_severity) + "</VIRUS_SEVERITY>\n")
			configuration_file.write("\t\t<RUN_IDX>0</RUN_IDX>\n")
			configuration_file.write("\t</environment>\n")

			configuration_file.write("</states>")

		autogenerated_variables_names.write("#define SEED \"SEED\"\n")
		autogenerated_variables_names.write("#define INDEX2COORDX \"INDEX2COORDX\"\n")
		autogenerated_variables_names.write("#define INDEX2COORDY \"INDEX2COORDY\"\n")
		autogenerated_variables_names.write("#define INDEX2COORDZ \"INDEX2COORDZ\"\n")
		autogenerated_variables_names.write("#define NODE_TYPE \"NODE_TYPE\"\n")
		autogenerated_variables_names.write("#define NODE_YAW \"NODE_YAW\"\n")
		autogenerated_variables_names.write("#define NODE_LENGTH \"NODE_LENGTH\"\n")
		autogenerated_variables_names.write("#define NODE_WIDTH \"NODE_WIDTH\"\n")
		autogenerated_variables_names.write("#define EXTERN_RANGES \"EXTERN_RANGES\"\n")
		autogenerated_variables_names.write("#define EXTERN_NODE \"EXTERN_NODE\"\n")
		autogenerated_variables_names.write("#define NEXT_CONTACTS_ID \"NEXT_CONTACTS_ID\"\n")
		autogenerated_variables_names.write("#define DAY \"DAY\"\n")
		autogenerated_variables_names.write("#define WEEK_DAY \"WEEK_DAY\"\n")
		autogenerated_variables_names.write("#define MEAN_INCUBATION_DAYS \"MEAN_INCUBATION_DAYS\"\n")
		autogenerated_variables_names.write("#define MEAN_INFECTION_DAYS \"MEAN_INFECTION_DAYS\"\n")
		autogenerated_variables_names.write("#define MEAN_END_OF_IMMUNIZATION_DAYS \"MEAN_END_OF_IMMUNIZATION_DAYS\"\n")
		autogenerated_variables_names.write("#define MEAN_FATALITY_DAYS \"MEAN_FATALITY_DAYS\"\n")
		autogenerated_variables_names.write("#define EXHALATION_MASK_EFFICACY \"EXHALATION_MASK_EFFICACY\"\n")
		autogenerated_variables_names.write("#define INHALATION_MASK_EFFICACY \"INHALATION_MASK_EFFICACY\"\n")
		autogenerated_variables_names.write("#define CONTAMINATION_RISK \"CONTAMINATION_RISK\"\n")
		autogenerated_variables_names.write("#define CONTAMINATION_RISK_DECREASED_WITH_MASK \"CONTAMINATION_RISK_DECREASED_WITH_MASK\"\n")
		autogenerated_variables_names.write("#define NGEN_BASE \"NGEN_BASE\"\n")
		autogenerated_variables_names.write("#define VL \"VL\"\n")
		autogenerated_variables_names.write("#define VIRUS_VARIANT_FACTOR \"VIRUS_VARIANT_FACTOR\"\n")
		autogenerated_variables_names.write("#define DECAY_RATE \"DECAY_RATE\"\n")
		autogenerated_variables_names.write("#define GRAVITATIONAL_SETTLING_RATE \"GRAVITATIONAL_SETTLING_RATE\"\n")
		autogenerated_variables_names.write("#define INHALATION_RATE_PURE \"INHALATION_RATE_PURE\"\n")
		autogenerated_variables_names.write("#define RISK_CONST \"RISK_CONST\"\n")
		autogenerated_variables_names.write("#define PERC_INF \"PERC_INF\"\n")
		autogenerated_variables_names.write("#define SENSITIVITY_SWAB \"SENSITIVITY_SWAB\"\n")
		autogenerated_variables_names.write("#define SPECIFICITY_SWAB \"SPECIFICITY_SWAB\"\n")
		autogenerated_variables_names.write("#define VIRUS_SEVERITY \"VIRUS_SEVERITY\"\n")
		autogenerated_variables_names.write("#define RUN_IDX \"RUN_IDX\"\n")

		if ensemble == "ON":
			autogenerated_defines.write("#define ENSEMBLE\n\n")

		autogenerated_defines.write("#define EXPERIMENT_NAME \"" + dirname_experiment + "\"\n\n")

		autogenerated_defines.write("#define NRUN " + str(nrun) + "\n\n")

		autogenerated_defines.write("#define AGENT_WITHOUT_RATE 0\n")
		autogenerated_defines.write("#define AGENT_WITH_RATE 1\n\n")

		autogenerated_defines.write("#define NUMBER_OF_AGENTS_TYPES_WITHOUT_A_RATE " + str(total_number_of_agents_types-nawar) + "\n")
		autogenerated_defines.write("#define NUMBER_OF_AGENTS_TYPES_WITH_A_RATE " + str(nawar) + "\n")
		autogenerated_defines.write("#define NUMBER_OF_AGENTS_WITHOUT_A_RATE " + str(agents_count) + "\n")
		autogenerated_defines.write("#define NUMBER_OF_AGENTS_TYPES " + str(total_number_of_agents_types) + "\n")
		autogenerated_defines.write("#define NUMBER_OF_AGENTS_TYPES_PLUS_1 " + str(total_number_of_agents_types+1) + "\n\n")
		for agent, ID in pedestrian_names.items():
			autogenerated_defines.write("#define " + str(agent.upper()) + " " + str(ID) + "\n")

		autogenerated_defines.write("\n")

		autogenerated_defines.write("#define WALL 0\n")
		autogenerated_defines.write("#define WALKABLE 1\n")
		autogenerated_defines.write("#define DOOR 2\n")
		autogenerated_defines.write("#define CORRIDOR 3\n")
		for me in MapEncoding:
			if me not in [MapEncoding.WALL, MapEncoding.WALKABLE, MapEncoding.DOOR, MapEncoding.CORRIDOR]:
				autogenerated_defines.write("#define " + MapEncoding.to_str(me) + " " + str(me.value) + "\n")
		
		autogenerated_defines.write("\n")

		autogenerated_defines.write("#define DISEASE_STATES " + str(len(compartments.keys())) + "\n")
		autogenerated_defines.write("#define SUSCEPTIBLE " + str(compartments["S"]) + "\n")
		autogenerated_defines.write("#define EXPOSED " + str(compartments["E"]) + "\n")
		autogenerated_defines.write("#define INFECTED " + str(compartments["I"]) + "\n")
		autogenerated_defines.write("#define RECOVERED " + str(compartments["R"]) + "\n")
		autogenerated_defines.write("#define DIED " + str(compartments["D"]) + "\n\n")

		autogenerated_defines.write("#define DETERMINISTIC " + str(distributions["Deterministic"]) + "\n")
		autogenerated_defines.write("#define EXPONENTIAL " + str(distributions["Exponential"]) + "\n")
		autogenerated_defines.write("#define UNIFORM " + str(distributions["Uniform"]) + "\n")
		autogenerated_defines.write("#define TRUNCATED_POSITIVE_NORMAL " + str(distributions["Truncated Positive Normal"]) + "\n\n")

		for mask_type, ID in mask_types.items():
			autogenerated_defines.write("#define " + mask_type.upper().replace(" ", "_") + " " + str(ID) + "\n")

		autogenerated_defines.write("\n")

		autogenerated_defines.write("#define YEXTERN " + str(int(entrance_y_coords)) + "\n")
		autogenerated_defines.write("#define AREAS_LENGTH " + str(areas_len_flows) + "\n\n")

		autogenerated_defines.write("#define MINOR 0\n")
		autogenerated_defines.write("#define MAJOR 1\n\n")

		autogenerated_defines.write("#define NOT_IDENTIFIED 0\n")
		autogenerated_defines.write("#define IDENTIFIED 1\n\n")

		autogenerated_defines.write("#define NO_SWAB -1\n")
		autogenerated_defines.write("#define NO_QUARANTINE -1\n")
		autogenerated_defines.write("#define NO_QUARANTINE_SWAB -1\n\n")

		autogenerated_defines.write("#define INSIDE_WAITING_ROOM 1\n")
		autogenerated_defines.write("#define OUTSIDE_WAITING_ROOM 0\n")
		autogenerated_defines.write("#define STAYING_IN_WAITING_ROOM 0\n")
		autogenerated_defines.write("#define EXITING_FROM_WAITING_ROOM 1\n")

		autogenerated_defines.write("#define NUM_COUNTERS " + str(num_counters) + "\n")
		autogenerated_defines.write("#define COUNTERS_CREATED_AGENTS_WITH_RATE 0\n")
		autogenerated_defines.write("#define COUNTERS_KILLED_AGENTS_WITH_RATE 1\n")
		autogenerated_defines.write("#define AGENTS_IN_QUARANTINE 2\n")
		autogenerated_defines.write("#define SWABS 3\n")
		autogenerated_defines.write("#define NUM_INFECTED_OUTSIDE 4\n\n")

		autogenerated_defines.write("#define VERY_LIGHT_ACTIVITY 1\n")
		autogenerated_defines.write("#define LIGHT_ACTIVITY 1.7777\n")
		autogenerated_defines.write("#define QUITE_HARD_ACTIVITY 2.5556\n")
		autogenerated_defines.write("#define HARD_ACTIVITY 6.1111\n\n")

		autogenerated_defines.write("#define PEDESTRIAN_UNIFORM_0_1_DISTR_IDX 0\n")
		autogenerated_defines.write("#define PEDESTRIAN_EVENT_DISTR_IDX 1\n")
		autogenerated_defines.write("#define PEDESTRIAN_JITTER_X_DISTR_IDX 2\n")
		autogenerated_defines.write("#define PEDESTRIAN_JITTER_Z_DISTR_IDX 3\n")
		autogenerated_defines.write("#define PEDESTRIAN_TAKE_NEW_DESTINATION_DISTR_IDX 4\n")
		autogenerated_defines.write("#define PEDESTRIAN_FLOW_DISTR_IDX 5\n")
		autogenerated_defines.write("#define PEDESTRIAN_QUARANTINE_DISTR_IDX 6\n")
		autogenerated_defines.write("#define PEDESTRIAN_QUARANTINE_SWAB_DISTR_IDX 7\n")
		autogenerated_defines.write("#define PEDESTRIAN_SWAB_DISTR_IDX 8\n")
		autogenerated_defines.write("#define PEDESTRIAN_HOURS_SCHEDULE_DISTR_IDX 9\n")
		autogenerated_defines.write("#define PEDESTRIAN_END_OF_IMMUNIZATION_DISTR_IDX 10\n")
		autogenerated_defines.write("#define PEDESTRIAN_INFECTION_DISTR_IDX 11\n")
		autogenerated_defines.write("#define PEDESTRIAN_FATALITY_DISTR_IDX 12\n")
		autogenerated_defines.write("#define PEDESTRIAN_INCUBATION_DISTR_IDX 13\n\n")

		autogenerated_defines.write("#define HOST_UNIFORM_0_1_DISTR_IDX 0\n")
		autogenerated_defines.write("#define HOST_RATE_DISTR_IDX 1\n")
		autogenerated_defines.write("#define HOST_OFFSET_X_DISTR_IDX 2\n")
		autogenerated_defines.write("#define HOST_OFFSET_Z_DISTR_IDX 3\n")
		autogenerated_defines.write("#define HOST_SWAB_DISTR_IDX 4\n")
		autogenerated_defines.write("#define HOST_FLOW_DISTR_IDX 5\n")
		autogenerated_defines.write("#define HOST_HOURS_SCHEDULE_DISTR_IDX 6\n")
		autogenerated_defines.write("#define HOST_END_OF_IMMUNIZATION_DISTR_IDX 7\n")
		autogenerated_defines.write("#define HOST_INFECTION_DISTR_IDX 8\n")
		autogenerated_defines.write("#define HOST_FATALITY_DISTR_IDX 9\n")

def main():
	parser = argparse.ArgumentParser()
	parser.add_argument('-dirname_experiment', type=str, help='Name of the directory with the configuration to run')
	parser.add_argument('-ensemble', type=str, help='Run with ensemble?')
	parser.add_argument('-checkpoint', type=str, help='Checkpoint simulation?')
	args = parser.parse_args()


	total_agents_overestimation = 10000
	solution_length = 50
	num_counters = 5
	y_offset = 10

	with open("f4f/" + args.dirname_experiment + "/WHOLEmodel.json") as file:
		WHOLEmodel = json.load(file)

	seed = int(WHOLEmodel["starting"][0]["seed"])
	# np.random.seed(seed)

	step = int(WHOLEmodel["starting"][0]["step"])

	rooms_info = obtain_rooms(WHOLEmodel)
	canvas_dimensions = obtain_canvas_dimension(WHOLEmodel)
	areas = obtain_areas(WHOLEmodel)

	steps_in_a_minute = 60 / step
	steps_in_a_hour = steps_in_a_minute * 60
	steps_in_a_day = steps_in_a_hour * 24

	init_week_day = WHOLEmodel["starting"][0]["day"]
	start_step_time = int(WHOLEmodel["starting"][0]["time"].split(":")[0]) * steps_in_a_hour + int(WHOLEmodel["starting"][0]["time"].split(":")[1]) * steps_in_a_minute
	start_step_time = 1 if start_step_time == 0 else start_step_time

	days = int(WHOLEmodel["starting"][0]["simulation_days"])

	with open("../src/autogenerated_variables_names.h", "w") as autogenerated_variables_names:
		autogenerated_variables_names.write("#ifndef _AUTOGENERATED_VARIABLES_NAMES_CUH_\n")
		autogenerated_variables_names.write("#define _AUTOGENERATED_VARIABLES_NAMES_CUH_\n\n")

		types = WHOLEmodel["types"]
		types_IDs = {}

		for type in types:
			name = type["Name"]
			ID = int(type["ID"])
			r, g, b, _ = type["Color"].replace("rgba(", "").replace(")", "").replace(" ", "").split(",")

			types_IDs[name] = {"ID": ID, "RGB": (int(float(r)), int(float(g)), int(float(b)))}

			autogenerated_variables_names.write("#define " + name.upper() + "_STRING \"" + name + "\"\n")
		
		autogenerated_variables_names.write("\n#define ROOM_AGENT_STRING \"room\"\n")
		autogenerated_variables_names.write("#define FILLINGROOM_AGENT_STRING \"fillingroom\"\n\n")

		floors = WHOLEmodel["floors"]
		floors_IDs = {}

		for floor in floors:
			name = floor["Name"]
			order = floor["Order"] - 1

			floors_IDs[name] = {"order": order}

		autogenerated_variables_names.write("\n")

		with open("../src/autogenerated_defines.h", "w") as autogenerated_defines:
			autogenerated_defines.write("#ifndef _AUTOGENERATED_DEFINES_CUH_\n")
			autogenerated_defines.write("#define _AUTOGENERATED_DEFINES_CUH_\n\n")

			autogenerated_defines.write("#define NUM_ROOMS_TYPES 2\n")
			autogenerated_defines.write("#define ROOMS {\"room\", \"fillingroom\"}\n\n")
			autogenerated_defines.write("#define NUM_ROOMS " + str(len(WHOLEmodel["roomsINcanvas"])) + "\n")

			color = WHOLEmodel["color"][0]

			if color == "Room":
				autogenerated_defines.write("#define NUM_COLORS " + str(len(rooms_info.keys())) + "\n")
				autogenerated_defines.write("#define COLORS {" + ', '.join(map(str, ["visualiser::Color{" + str(value["RGB"][0]) + ", " + str(value["RGB"][1]) + ", " + str(value["RGB"][2]) + "}" for _, value in rooms_info.items()])) + "}\n")
			elif color == "Type":
				autogenerated_defines.write("#define NUM_COLORS " + str(len(types_IDs.keys())) + "\n")
				autogenerated_defines.write("#define COLORS {" + ', '.join(map(str, ["visualiser::Color{" + str(value["RGB"][0]) + ", " + str(value["RGB"][1]) + ", " + str(value["RGB"][2]) + "}" for _, value in types_IDs.items()])) + "}\n")
			else:
				autogenerated_defines.write("#define NUM_COLORS " + str(len(areas.keys())) + "\n")
				autogenerated_defines.write("#define COLORS {" + ', '.join(map(str, ["visualiser::Color{" + str(value["RGB"][0]) + ", " + str(value["RGB"][1]) + ", " + str(value["RGB"][2]) + "}" for _, value in areas.items()])) + "}\n\n")

			autogenerated_defines.write("#define SOLUTION_LENGTH " + str(solution_length) + "\n\n")

			autogenerated_defines.write("#define STEP " + str(step) + "\n")
			autogenerated_defines.write("#define START_STEP_TIME " + str(int(start_step_time)) + "\n")
			autogenerated_defines.write("#define STEPS_IN_A_DAY " + str(int(steps_in_a_day)) + "\n")
			autogenerated_defines.write("#define STEPS_IN_A_HOUR " + str(int(steps_in_a_hour)) + "\n")
			autogenerated_defines.write("#define STEPS_IN_A_MINUTE " + str(int(steps_in_a_minute)) + "\n\n")

			# XML definitions
			if args.ensemble == "ON":
				input_file = "run_plan_vector.json"
			else:
				input_file = "configuration_file.xml"

			pedestrian_names = dict()
			agents = WHOLEmodel["agents"]
			filtered_agents_time_window = {agent: details for agent, details in agents.items() if details['entry_type'][0] == 'Time window'}
			filtered_agents_daily_rate = {agent: details for agent, details in agents.items() if details['entry_type'][0] == 'Daily Rate'}
			agents = filtered_agents_time_window | filtered_agents_daily_rate
			for agent_name, _ in agents.items():
				pedestrian_names[agent_name] = len(pedestrian_names.keys())

			environment_dimensions = (int(canvas_dimensions["x"]), (len(floors_IDs) + 1) * y_offset, int(canvas_dimensions["z"]))

			generate_xml(input_file, seed, rooms_info, areas, pedestrian_names, agents, args.ensemble, args.checkpoint,
						environment_dimensions, total_agents_overestimation, solution_length, y_offset, args.dirname_experiment + "/",
						WHOLEmodel,	autogenerated_defines, autogenerated_variables_names, days, floors_IDs, types_IDs,
						steps_in_a_day, steps_in_a_hour, steps_in_a_minute, init_week_day, start_step_time, num_counters)

			autogenerated_defines.write("#endif //_AUTOGENERATED_DEFINES_CUH_\n")

		autogenerated_variables_names.write("#endif //_AUTOGENERATED_VARIABLES_NAMES_CUH_\n")

	return seed

seed = main()
print(str(seed))
