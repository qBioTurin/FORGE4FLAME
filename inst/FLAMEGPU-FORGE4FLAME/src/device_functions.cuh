#ifndef _DEVICE_FUNCTIONS_CUH_
#define _DEVICE_FUNCTIONS_CUH_

#include "defines.h"

using namespace flamegpu;
using namespace std;

namespace device_functions {
    /** 
     * Compare floating points.
    */
    FLAMEGPU_DEVICE_FUNCTION bool compare_float(const double a, const double b, const double epsilon) {
        return fabs(a - b) < epsilon;
    }

    /** 
     * Generate a random number using the given RNG, distribution and parameters for pedestrians.
    */
    FLAMEGPU_DEVICE_FUNCTION float cuda_pedestrian_rng(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, unsigned short distribution_id, curandState *cuda_states, int type, short id, int a, int b, bool flow_time) {
        float random = (type == TRUNCATED_POSITIVE_NORMAL) ? curand_normal(&cuda_states[id]): curand_uniform(&cuda_states[id]);
        
        if(type == EXPONENTIAL && compare_float((double) random, 1.0f, 1e-10)){
            do{
                random = curand_uniform(&cuda_states[id]);
            }while(compare_float((double) random, 1.0f, 1e-10));
        }
        const float event_time_random = DISTRIBUTION(type, random, a, b);

        //printf("[RANDOM_AGENT],%d,%d,%d,%d,%d,%d,%d,%f,%s\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), distribution_id, id, type, a, b, (flow_time && event_time_random < 1.0f) ? 1.0f: event_time_random, flow_time ? "true" : "false");
        // printf("[RANDOM_AGENT],%d,%d,%d,%d,%d,%d,%d,%f,%s\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), distribution_id, id, type, a, b, (flow_time && event_time_random < 1.0f) ? 1.0f: event_time_random, flow_time ? "true" : "false");

        auto cuda_rng_offsets_pedestrian = FLAMEGPU->environment.getMacroProperty<unsigned int, TOTAL_AGENTS_OVERESTIMATION>(CUDA_RNG_OFFSETS_PEDESTRIAN);
        cuda_rng_offsets_pedestrian[FLAMEGPU->getVariable<short>(CONTACTS_ID)]++;

        return (flow_time && event_time_random < 1.0f) ? 1.0f: event_time_random;
    }

    /** 
     * Generate a random number using the given RNG, distribution and parameters for rooms.
    */
    FLAMEGPU_DEVICE_FUNCTION float cuda_room_rng(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, unsigned short distribution_id, curandState *cuda_states, int type, short id, int a, int b, bool flow_time) {
        float random = (type == TRUNCATED_POSITIVE_NORMAL) ? curand_normal(&cuda_states[id]): curand_uniform(&cuda_states[id]);

        if(type == EXPONENTIAL && compare_float((double) random, 1.0f, 1e-10)){
            do{
                random = curand_uniform(&cuda_states[id]);
            }while(compare_float((double) random, 1.0f, 1e-10));
        }
        const float event_time_random = DISTRIBUTION(type, random, a, b);

        // printf("[RANDOM_ROOM],%d,%d,%d,%d,%d,%d,%f,%s\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), distribution_id, id, type, a, b, (flow_time && event_time_random < 1.0f) ? 1.0f: event_time_random, flow_time ? "true" : "false");

        auto cuda_rng_offsets_room = FLAMEGPU->environment.getMacroProperty<unsigned int, NUM_ROOMS>(CUDA_RNG_OFFSETS_ROOM);
        cuda_rng_offsets_room[FLAMEGPU->getID()]++;

        return (flow_time && event_time_random < 1.0f) ? 1.0f: event_time_random;
    }

    /** 
     * Generate a random offset inside rooms.
    */
    FLAMEGPU_DEVICE_FUNCTION void generate_offset(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, float* jitter_x, float* jitter_z,  short new_target){
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
        const float yaw = FLAMEGPU->environment.getProperty<float, V>(NODE_YAW, new_target);
        const bool yaw_condition = compare_float(yaw, M_PI/2, 0.5f) || compare_float(yaw, 2*M_PI - M_PI/2, 0.5f);

        int length = FLAMEGPU->environment.getProperty<int, V>(NODE_LENGTH, new_target);
        int width = FLAMEGPU->environment.getProperty<int, V>(NODE_WIDTH, new_target);
        float offset_x = 0.0f;
        float offset_z = 0.0f;
        char even_offset_length = 0;
        char even_offset_width = 0;

        even_offset_length = (length % 2 == 0) ? 1: 0;
        even_offset_width = (width % 2 == 0) ? 1: 0;

        length = length + even_offset_length;
        width = width + even_offset_width;

        offset_x = ((yaw_condition) ? width: length) - 1;
        offset_z = ((yaw_condition) ? length: width) - 1;

        if(compare_float(yaw, 0, 0.5f)){
            *jitter_x = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_X_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_x/2, offset_x/2 - even_offset_length, false);
            *jitter_z = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_Z_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_z/2 + even_offset_width, offset_z / 2, false);
        }
        else if(compare_float(yaw, M_PI/2, 0.5f)){
            *jitter_x = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_X_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_x/2, offset_x/2 - even_offset_width, false);
            *jitter_z = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_Z_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_z/2, offset_z/2 - even_offset_length, false);
        }
        else if(compare_float(yaw, M_PI, 0.5f)){
            *jitter_x = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_X_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_x/2 + even_offset_length, offset_x/2, false);
            *jitter_z = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_Z_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_z/2, offset_z/2 - even_offset_width, false);
        }
        else{
            *jitter_x = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_X_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_x/2 + even_offset_width, offset_x/2, false);
            *jitter_z = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_JITTER_Z_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, -offset_z/2 + even_offset_length, offset_z/2, false);
        }
    }


    FLAMEGPU_DEVICE_FUNCTION short findFreeRoomOfTypeAndArea(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, int flow, int random, int lenght_rooms, unsigned short* ward_indeces, bool *available) {  
        
        int random_iterator = random;
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
        short final_target = FLAMEGPU->environment.getProperty<unsigned short>(EXTERN_NODE);
        //resources
        auto global_resources = FLAMEGPU->environment.getMacroProperty<int, V>(GLOBAL_RESOURCES);
        auto global_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, V>(GLOBAL_RESOURCES_COUNTER);
        auto specific_resources = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES);
        auto specific_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES_COUNTER);
        
        auto messages = FLAMEGPU->message_in(flow);

        do {

            auto list_front = messages.begin();

            for(int i = 0; i < ward_indeces[random_iterator]; i++) list_front++;

            final_target = (*list_front).getVariable<short>(GRAPH_NODE);
        
            // Try getting the resources of the room
            int get_global_resource = ++global_resources_counter[final_target];
            int get_specific_resources = ++specific_resources_counter[agent_type][final_target];

            if(get_global_resource > global_resources[final_target] || get_specific_resources > specific_resources_counter[agent_type][final_target]){
                unsigned int global = --global_resources_counter[final_target];
                unsigned int specific = --specific_resources_counter[agent_type][final_target];
                random_iterator = (random_iterator + 1) % lenght_rooms;
            }
            else {
                *available = true;
            }
        }
        while(!available && random_iterator != random);  

        return final_target;
     }


    /** 
     * Take the next destination inside the determined flow of the agent.
    */
    FLAMEGPU_DEVICE_FUNCTION short take_new_destination_flow(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, int *stay, const bool identified = false, const unsigned short severity = MINOR){  
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning of take_new_destination_flow for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        unsigned short flow_index = FLAMEGPU->getVariable<unsigned short>(FLOW_INDEX) + 1;
        unsigned short week_day_flow = FLAMEGPU->getVariable<unsigned short>(WEEK_DAY_FLOW);
        short final_target = FLAMEGPU->environment.getProperty<unsigned short>(EXTERN_NODE);

        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);

        auto env_room_for_quarantine_type = FLAMEGPU->environment.getMacroProperty<int>(ENV_ROOM_FOR_QUARANTINE_TYPE);
        auto env_room_for_quarantine_area = FLAMEGPU->environment.getMacroProperty<int>(ENV_ROOM_FOR_QUARANTINE_AREA);
        auto env_flow = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW);
        auto env_flow_distr = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR);
        auto env_flow_distr_firstparam = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR_FIRSTPARAM);
        auto env_flow_distr_secondparam = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR_SECONDPARAM);
        auto env_flow_area = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_AREA);
        //resources
        auto global_resources = FLAMEGPU->environment.getMacroProperty<int, V>(GLOBAL_RESOURCES);
        auto global_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, V>(GLOBAL_RESOURCES_COUNTER);
        auto specific_resources = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES);
        auto specific_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES_COUNTER);
        auto alternative_resources_area = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(ALTERNATIVE_RESOURCES_AREA);
        auto alternative_resources_type = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(ALTERNATIVE_RESOURCES_TYPE);

        const int flow = (int) env_flow[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH];
        const int flow_area = (int) env_flow_area[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH];

        FLAMEGPU->setVariable<unsigned short>(FLOW_INDEX, flow_index);

        const bool isValidFlow = (flow != -1 && flow != SPAWNROOM);

        if(env_flow_distr[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH] != -1)
            *stay = (unsigned int) cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_FLOW_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], env_flow_distr[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH], contacts_id, env_flow_distr_firstparam[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH], env_flow_distr_secondparam[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH], true);

        unsigned short j = 0;
        if(isValidFlow && ((severity == MAJOR && (int) env_room_for_quarantine_type[agent_type] != SPAWNROOM) || identified == NOT_IDENTIFIED)){
            auto messages = FLAMEGPU->message_in(flow);
            int area = flow_area;

            if(severity == MAJOR){
                messages = FLAMEGPU->message_in((int) env_room_for_quarantine_type[agent_type]);
                area = (int) env_room_for_quarantine_area[agent_type];
            }

            unsigned short ward_indeces[SOLUTION_LENGTH];

            auto i = messages.begin();
            unsigned short k = 0;
            while(i != messages.end()){
                const int local_area = (*i).getVariable<int>(AREA);

                if(local_area == area){
                    ward_indeces[j] = k;
                    j++;
                }

                i++;
                k++;
            }

            const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);

            int random = round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_TAKE_NEW_DESTINATION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, j-1, false));
            int random_iterator = random;
            int lenght_rooms = j;
            bool available = false;
            unsigned int get_global_resource;
            unsigned int get_specific_resources;
            short nodo = FLAMEGPU->getVariable<short>(NODE_WAITING_FOR);

            //if the agent is already waiting for a node, go for it
            if(FLAMEGPU->getVariable<short>(NODE_WAITING_FOR) != -1){

                final_target = FLAMEGPU->getVariable<short>(NODE_WAITING_FOR);
            }
            else {
                auto list_front = messages.begin();

                for(int i = 0; i < ward_indeces[random]; i++) list_front++;

                final_target = (*list_front).getVariable<short>(GRAPH_NODE);
            }

            if(alternative_resources_type[agent_type][final_target] == WAITINGROOM && FLAMEGPU->getVariable<int>(WAITING_ROOM_FLAG) == OUTSIDE_WAITING_ROOM){
                
                float agent_pos[3] = {FLAMEGPU->getVariable<float>(X), FLAMEGPU->getVariable<float>(Y), FLAMEGPU->getVariable<float>(Z)};
                FLAMEGPU->setVariable<short>(NODE_WAITING_FOR, final_target);
                float min_separation = FLT_MAX;

                for(const auto& message: FLAMEGPU->message_in(WAITINGROOM)) {
                    const unsigned short near_agent_pos[3] = {message.getVariable<unsigned short>(X), message.getVariable<unsigned short>(Y), message.getVariable<unsigned short>(Z)};

                    float separation = abs(near_agent_pos[0] - agent_pos[0]) + abs(near_agent_pos[1] - agent_pos[1]) + abs(near_agent_pos[2] - agent_pos[2]);
                    if(separation < min_separation){ 
                        min_separation = separation;
                        final_target = message.getVariable<short>(GRAPH_NODE);
                    }
                }

                FLAMEGPU->setVariable<int>(WAITING_ROOM_FLAG, INSIDE_WAITING_ROOM);
                FLAMEGPU->setVariable<int>(ENTRY_EXIT_FLAG, STAYING_IN_WAITING_ROOM);
                *stay = 2;
            }
            else if(alternative_resources_type[agent_type][final_target] == WAITINGROOM && FLAMEGPU->getVariable<int>(WAITING_ROOM_FLAG) == INSIDE_WAITING_ROOM){
                
                //The agent have waited in waiting room and now go to the right flux room
                FLAMEGPU->setVariable<int>(WAITING_ROOM_FLAG, OUTSIDE_WAITING_ROOM);
                final_target = FLAMEGPU->getVariable<short>(NODE_WAITING_FOR);
                FLAMEGPU->setVariable<short>(NODE_WAITING_FOR, -1);
            
            }
            else if(alternative_resources_type[agent_type][final_target] != WAITINGROOM && alternative_resources_type[agent_type][final_target] != -1){
                
                // Try getting the resources of the room
                get_global_resource = ++global_resources_counter[final_target];
                get_specific_resources = ++specific_resources_counter[agent_type][final_target];
                int global_resources_n = global_resources[final_target];
                int specific_resources_n = specific_resources[agent_type][final_target];

                if(get_global_resource <= global_resources[final_target] && get_specific_resources <= specific_resources[agent_type][final_target]){
                   available = true; 
                } 

                //if the initial room is not avaiable because the resources are over, explore the alternatives:
                if(!available){

                    get_global_resource = --global_resources_counter[final_target]; 
                    get_specific_resources = --specific_resources_counter[agent_type][final_target];

                    //search another room of the same type and area
                    if(alternative_resources_area[agent_type][final_target] == area && alternative_resources_type[agent_type][final_target] == flow){

                        random = (random + 1) % lenght_rooms;
                        final_target = findFreeRoomOfTypeAndArea(FLAMEGPU, flow, random, lenght_rooms, ward_indeces, &available);

                    }
                    //search another room of the alternative
                    else if(alternative_resources_area[agent_type][final_target] == area || alternative_resources_type[agent_type][final_target] == flow){
                        
                        auto messages = FLAMEGPU->message_in(alternative_resources_type[agent_type][final_target]);

                        unsigned short ward_indeces_alternative[SOLUTION_LENGTH];
                        unsigned short j = 0;
                        unsigned short k = 0;

                        auto i = messages.begin();
                        while(i != messages.end()){
                            const int local_area = (*i).getVariable<int>(AREA);

                            if(local_area == alternative_resources_area[agent_type][final_target]){
                                ward_indeces_alternative[j] = k;
                                j++;
                            }

                            i++;
                            k++;
                        }

                        int random = round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_TAKE_NEW_DESTINATION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, j-1, false));
                        final_target = findFreeRoomOfTypeAndArea(FLAMEGPU, alternative_resources_type[agent_type][final_target], random, lenght_rooms, ward_indeces_alternative, &available);
                    }
                }

                //if no other alternave is avaiable or it's explicit, skip
                if(!available || alternative_resources_type[agent_type][final_target] == -1){
                
                    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);
                    const float final_target_vec[3] = {FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 0), FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 1), FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 2)};
                    final_target = coord2index[(unsigned short)(final_target_vec[1]/YOFFSET)][(unsigned short)final_target_vec[2]][(unsigned short)final_target_vec[0]];
                    *stay = 1;
                }
            }
        }

        FLAMEGPU->setVariable<float, 3>(FINAL_TARGET, 0, FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDX, final_target));
        FLAMEGPU->setVariable<float, 3>(FINAL_TARGET, 1, FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDY, final_target));
        FLAMEGPU->setVariable<float, 3>(FINAL_TARGET, 2, FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDZ, final_target));

        if(identified == IDENTIFIED)
            FLAMEGPU->setVariable<int>(ROOM_FOR_QUARANTINE_INDEX, final_target);

        if((int) env_flow[agent_type][week_day_flow][(flow_index + 1) % SOLUTION_LENGTH] == -1 && final_target == FLAMEGPU->environment.getProperty<unsigned short>(EXTERN_NODE))
            *stay = 1;

#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending of take_new_destination_flow for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        return final_target;
    }

    /** 
     * Find the shortest path between two nodes in the graph.
    */
    FLAMEGPU_DEVICE_FUNCTION void a_star(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, const unsigned short start, const unsigned short goal, short* solution) {    
    
    printf("cosa cosa, in astar entri?\n");

#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning of a_star for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif        
        short closedset[V];
        short openset[V][3];

        for (unsigned short i = 0; i < V; ++i) {
            closedset[i] = NOT_PRESENT; 
            openset[i][0] = NOT_PRESENT;
            openset[i][1] = NOT_PRESENT;
            openset[i][2] = NOT_PRESENT;
        }

        float x_start = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDX, start);
        float x_goal = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDX, goal);
        float z_start = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDZ, start);
        float z_goal = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDZ, goal);

        //Initialize the starting node
        short initial_h = MANHATTAN_DISTANCE(x_start, x_goal, z_start, z_goal);
        openset[start][0] = initial_h;
        openset[start][1] = 0;
        openset[start][2] = STARTING_POINT;

        auto adjmatrix = FLAMEGPU->environment.getMacroProperty<unsigned short, V, V>(ADJMATRIX);

        // Keep looping WHILE there are elements in the open set 
        for(unsigned short n_open = 1; n_open;) {
            //1a. Identify next node to be expanded!
            short next_vertex = NOT_PRESENT;

            for(short i = 0, curr_f_cost; i < V; ++i) {
                if((curr_f_cost = openset[i][F_COST]) != NOT_PRESENT) {
                    if(next_vertex == NOT_PRESENT || curr_f_cost < openset[next_vertex][F_COST]) {
                        next_vertex = i;
                    }
                }
            }
            
            //1b. Pick the selected node and remove it from the openset 
            short curr_node[3];
            curr_node[0] = openset[next_vertex][0];
            curr_node[1] = openset[next_vertex][1];
            curr_node[2] = openset[next_vertex][2];

            openset[next_vertex][0] = NOT_PRESENT;
            openset[next_vertex][1] = NOT_PRESENT;
            openset[next_vertex][2] = NOT_PRESENT;
            --n_open;


            //2. Check if it matches with the goal 
            if(next_vertex == goal) {
                short backward_solution[SOLUTION_LENGTH], length = 0;

                closedset[next_vertex] = curr_node[PREV];

                for(short backtrack = closedset[next_vertex]; backtrack != STARTING_POINT; backtrack = closedset[backtrack])
                    backward_solution[length++] = backtrack;
                
                for(unsigned short i = 0, j = length - 1; i < length; ++i, --j){
                    solution[i] = backward_solution[j];
                }
                solution[length] = goal;
                for(unsigned short i = length+1; i < SOLUTION_LENGTH; ++i)
                    solution[i] = -1;
                return;
            }

            //3. Check if it is already visited (is it in the closedset?)
            if(closedset[next_vertex] == NOT_PRESENT) {
                short curr_x = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDX, next_vertex), curr_z = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDZ, next_vertex);
                //4. Add each unvisited neighbor of the current node to the openset 
                for(unsigned short i = 0; i < V; ++i) {
                    unsigned short we = adjmatrix[i][next_vertex];
                    // Foreach unvisited neighbor
                    if(we > 0 && closedset[i] == NOT_PRESENT) {
                        short new_g = curr_node[G_COST] + we;
                        short g_cost_neighbor = openset[i][G_COST];
                        /* A new node is added to the open set if 
                        it not present yet in the openset, 
                        or if the new cost is less than the current present in the openset.  */
                        bool first_time = g_cost_neighbor == NOT_PRESENT, 
                            not_new_but_interesting = new_g < g_cost_neighbor;
                        
                        //If it is the first time, the node is added to the openset and the number of elements is ++increased 
                        if((first_time && ++n_open) || not_new_but_interesting) {
                            // Add new node or replace node in openset estimating f(n)
                            float x_coord_i = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDX, i);
                            float z_coord_i = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDZ, i);
                            openset[i][0] = new_g + MANHATTAN_DISTANCE(x_coord_i, x_goal, z_coord_i, z_goal);
                            openset[i][1] = new_g;
                            openset[i][2] = next_vertex;
                        }
                    }
                }

                closedset[next_vertex] = curr_node[PREV];
            }
        }

#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending a_star for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif   
    }

    /** 
     * Update agent intermediate and final targets.
    */
    FLAMEGPU_DEVICE_FUNCTION void update_targets(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, short* new_targets, unsigned short *target_index, const bool clean, int stay) {
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning of update_targets for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif        
        float new_target_x, new_target_y, new_target_z;

        if(clean){
            const unsigned short next_index = FLAMEGPU->getVariable<unsigned short>(NEXT_INDEX);

            FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, *target_index, 0);
            
            if(next_index != *target_index){
                *target_index = (next_index + 1) % SOLUTION_LENGTH;
            }
        }

        short i = 1;
        while(i < SOLUTION_LENGTH && new_targets[i] != -1){
            float jitter_x = 0.0f;
            float jitter_z = 0.0f;

            // Generate a random offset
            if(i+1 < SOLUTION_LENGTH && new_targets[i+1] == -1){
                generate_offset(FLAMEGPU, &jitter_x, &jitter_z, new_targets[i]);
            }
            
            new_target_x = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDX, new_targets[i]) + 0.5f + jitter_x;
            new_target_y = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDY, new_targets[i]);
            new_target_z = FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDZ, new_targets[i]) + 0.5f + jitter_z;

            FLAMEGPU->setVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_X, *target_index, new_target_x);
            FLAMEGPU->setVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Y, *target_index, new_target_y);
            FLAMEGPU->setVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Z, *target_index, new_target_z);
            
            *target_index = (*target_index + 1) % SOLUTION_LENGTH;
            
            ++i;
        }

        FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, *target_index, stay);
        FLAMEGPU->setVariable<unsigned short>(TARGET_INDEX, *target_index);

#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending update_targets for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * Update agent's flow for the next day in which the agent will enter in the environment.
    */
    FLAMEGPU_DEVICE_FUNCTION void update_flow(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, const bool quarantine){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning of update_flow for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
        const unsigned short target_index = FLAMEGPU->getVariable<unsigned short>(TARGET_INDEX);
        const unsigned short identified = FLAMEGPU->getVariable<unsigned short>(IDENTIFIED_INFECTED);

        auto env_flow = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW);
        auto env_flow_distr = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR);
        auto env_flow_distr_firstparam = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR_FIRSTPARAM);
        auto env_flow_distr_secondparam = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR_SECONDPARAM);
        auto env_hours_schedule = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_HOURS_SCHEDULE);
        
        unsigned short entry_time_index = FLAMEGPU->getVariable<unsigned short>(ENTRY_TIME_INDEX) + 1;
        unsigned short week_day_flow = FLAMEGPU->getVariable<unsigned short>(WEEK_DAY_FLOW);
        unsigned short empty_days;
        int start_step;

        if((int) env_hours_schedule[agent_type][week_day_flow][2 * entry_time_index] == 0 || quarantine){
            entry_time_index = 0;
            week_day_flow = (week_day_flow + 1) % DAYS_IN_A_WEEK;
            
            empty_days = 0;
            while((int) env_flow[agent_type][week_day_flow][0] == -1){
                empty_days++;
                week_day_flow = (week_day_flow + 1) % DAYS_IN_A_WEEK;
            }

            start_step = (int) env_hours_schedule[agent_type][week_day_flow][0];
        }
        else{
            start_step = ((int) env_hours_schedule[agent_type][week_day_flow][2 * entry_time_index] - ((FLAMEGPU->getStepCounter() + START_STEP_TIME) % STEPS_IN_A_DAY));
        }

        if(entry_time_index == 0){
            FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, (unsigned int) (empty_days * STEPS_IN_A_DAY + (STEPS_IN_A_DAY - ((FLAMEGPU->getStepCounter() + START_STEP_TIME) % STEPS_IN_A_DAY)) + start_step));
            FLAMEGPU->setVariable<unsigned short>(FLOW_INDEX, week_day_flow * SOLUTION_LENGTH);
        }
        else{
            FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, (unsigned short) start_step);
        }

        FLAMEGPU->setVariable<unsigned short>(ENTRY_TIME_INDEX, entry_time_index);
        FLAMEGPU->setVariable<unsigned short>(WEEK_DAY_FLOW, week_day_flow);
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending update_flow for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    FLAMEGPU_DEVICE_FUNCTION void put_in_quarantine(DeviceAPI<MessageBucket, MessageNone> *FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning put_in_quarantine for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);

        auto env_quarantine_days_distr = FLAMEGPU->environment.getMacroProperty<int>(ENV_QUARANTINE_DAYS_DISTR);
        auto env_quarantine_days_distr_firstparam = FLAMEGPU->environment.getMacroProperty<int>(ENV_QUARANTINE_DAYS_DISTR_FIRSTPARAM);
        auto env_quarantine_days_distr_secondparam = FLAMEGPU->environment.getMacroProperty<int>(ENV_QUARANTINE_DAYS_DISTR_SECONDPARAM);
        auto env_quarantine_swab_days_distr = FLAMEGPU->environment.getMacroProperty<int>(ENV_QUARANTINE_SWAB_DAYS_DISTR);
        auto env_quarantine_swab_days_distr_firstparam = FLAMEGPU->environment.getMacroProperty<float>(ENV_QUARANTINE_SWAB_DAYS_DISTR_FIRSTPARAM);
        auto env_quarantine_swab_days_distr_secondparam = FLAMEGPU->environment.getMacroProperty<float>(ENV_QUARANTINE_SWAB_DAYS_DISTR_SECONDPARAM);

        unsigned short quarantine = FLAMEGPU->getVariable<unsigned short>(QUARANTINE);
        unsigned short identified_bool = FLAMEGPU->getVariable<unsigned short>(IDENTIFIED_INFECTED);
        unsigned short severity = FLAMEGPU->getVariable<unsigned short>(SEVERITY);
        unsigned short target_index = FLAMEGPU->getVariable<unsigned short>(TARGET_INDEX);
        bool already_in_quarantine = quarantine > 0;
        
        quarantine = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_QUARANTINE_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], (int) env_quarantine_days_distr[agent_type], contacts_id, (int) env_quarantine_days_distr_firstparam[agent_type], (int) env_quarantine_days_distr_secondparam[agent_type], true);
        FLAMEGPU->setVariable<unsigned short>(QUARANTINE, quarantine);

        int stay = 1;

        auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);

        const float final_target[3] = {FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 0), FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 1), FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 2)};
        const unsigned short agent_with_a_rate = FLAMEGPU->getVariable<unsigned short>(AGENT_WITH_A_RATE);
        
        FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, 0);

        printf("[QUARANTINE],%d,%d,%d,%d,%d,0\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), already_in_quarantine ? 1: 0, severity);

        if(!already_in_quarantine){
            const short start_node = coord2index[(unsigned short)(final_target[1]/YOFFSET)][(unsigned short)final_target[2]][(unsigned short)final_target[0]];
            const short quarantine_node = take_new_destination_flow(FLAMEGPU, &stay, identified_bool, severity);

            auto counters = FLAMEGPU->environment.getMacroProperty<unsigned int, NUM_COUNTERS>(COUNTERS);

            if(quarantine_node != FLAMEGPU->environment.getProperty<unsigned short>(EXTERN_NODE) && !FLAMEGPU->getVariable<unsigned char>(INIT)){
                FLAMEGPU->setVariable<unsigned char>(INIT, 1);
                FLAMEGPU->setVariable<float>(Y, YEXTERN);
            }

            short solution_start_quarantine[SOLUTION_LENGTH] = {-1};

            a_star(FLAMEGPU, start_node, quarantine_node, solution_start_quarantine);
            
            update_targets(FLAMEGPU, solution_start_quarantine, &target_index, false, quarantine * STEPS_IN_A_DAY);

            counters[AGENTS_IN_QUARANTINE]++;
        }
        else
            FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, quarantine * STEPS_IN_A_DAY);

        if(agent_with_a_rate)
            FLAMEGPU->setVariable<unsigned short>(FLOW_INDEX, FLAMEGPU->getVariable<unsigned short>(WEEK_DAY_FLOW) * SOLUTION_LENGTH);

        if((int) env_quarantine_swab_days_distr[agent_type] != NO_SWAB){
            int swab_steps = round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_QUARANTINE_SWAB_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], (int) env_quarantine_swab_days_distr[agent_type], contacts_id, STEPS_IN_A_DAY * (float) env_quarantine_swab_days_distr_firstparam[agent_type], STEPS_IN_A_DAY * (float) env_quarantine_swab_days_distr_secondparam[agent_type], true));
            FLAMEGPU->setVariable<int>(SWAB_STEPS, swab_steps);
        }
        
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending put_in_quarantine for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * Make a swab to the agent and handle quarantine.
    */
    FLAMEGPU_DEVICE_FUNCTION void swab(DeviceAPI<MessageBucket, MessageNone> *FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning swab for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const unsigned int disease_state = FLAMEGPU->getVariable<unsigned int>(DISEASE_STATE);
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);

        auto env_swab_distr = FLAMEGPU->environment.getMacroProperty<int>(ENV_SWAB_DISTR);
        auto env_swab_distr_firstparam = FLAMEGPU->environment.getMacroProperty<float>(ENV_SWAB_DISTR_FIRSTPARAM);
        auto env_swab_distr_secondparam = FLAMEGPU->environment.getMacroProperty<float>(ENV_SWAB_DISTR_SECONDPARAM);
        auto env_quarantine_days_distr = FLAMEGPU->environment.getMacroProperty<int>(ENV_QUARANTINE_DAYS_DISTR);
        auto counters = FLAMEGPU->environment.getMacroProperty<unsigned int, NUM_COUNTERS>(COUNTERS);

        unsigned short identified_bool = FLAMEGPU->getVariable<unsigned short>(IDENTIFIED_INFECTED);
        unsigned short quarantine = FLAMEGPU->getVariable<unsigned short>(QUARANTINE);
        unsigned short severity = FLAMEGPU->getVariable<unsigned short>(SEVERITY);
        unsigned short target_index = FLAMEGPU->getVariable<unsigned short>(TARGET_INDEX);
        bool already_in_quarantine = quarantine > 0;

        if(disease_state == INFECTED){
            const float sensitivity_swab = FLAMEGPU->environment.getProperty<float>(SENSITIVITY_SWAB);
            float random_sensitivity = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);

            if(random_sensitivity < sensitivity_swab){
                // True positive
                if(!already_in_quarantine){
                    const float severity_covid = FLAMEGPU->environment.getProperty<float>(VIRUS_SEVERITY);
                    float random_severity = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);

                    if(random_severity < severity_covid)
                        severity = MAJOR;
                }
                identified_bool = IDENTIFIED;
            }
            else{
                // False negative if random_sensitivity is greater (>) than sensitivity_swab
                identified_bool = NOT_IDENTIFIED;
            }
        }
        else {
            // False positive
            const float specificity_swab = FLAMEGPU->environment.getProperty<float>(SPECIFICITY_SWAB);
            float random_specificity = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);

            if(random_specificity >= specificity_swab){
                identified_bool = IDENTIFIED;
            }
            else{
                // True negative if random_specificity less or equal (<=) than specificity_swab
                identified_bool = NOT_IDENTIFIED;
            }
        }

        FLAMEGPU->setVariable<unsigned short>(IDENTIFIED_INFECTED, identified_bool);
        FLAMEGPU->setVariable<unsigned short>(SEVERITY, severity);

        counters[SWABS]++;

        // Now the agent could has been identified as infected
        if(identified_bool == IDENTIFIED){
            /**
             * Identified (True or False Positive), we have different cases:
             *  - No quarantine: do nothing
             *  - Quarantine and no swab during quarantine: put the agent in quarantine in the given room for n days
             *                                              (where n is generated using the selected distribution and parameters).
             *  - Quarantine and swab during quarantine: put the agent in quarantine in the given room for n days
             *                                           (where n is generated using the selected distribution and parameters) and
             *                                           make a swab every m days (where m is generated using the selected distribution
             *                                           and parameters). A Negative swab will allow the agent to exit from the quarantine.
             */
            FLAMEGPU->setVariable<int>(SWAB_STEPS, -1);

            if((int) env_quarantine_days_distr[agent_type] != NO_QUARANTINE){
                put_in_quarantine(FLAMEGPU);
            }
        }
        else{
            // Not identified (True or False Negative): the agent exits from quarantine
            if(already_in_quarantine){
                FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, 1);
            }
            else{
                if((int) env_swab_distr[agent_type] != NO_SWAB){
                    int swab_steps = round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_SWAB_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], (int) env_swab_distr[agent_type], contacts_id, STEPS_IN_A_DAY * (float) env_swab_distr_firstparam[agent_type], STEPS_IN_A_DAY * (float) env_swab_distr_secondparam[agent_type], true));
                    FLAMEGPU->setVariable<int>(SWAB_STEPS, swab_steps);
                }
            }
        }

#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending swab for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * The agent exits from quarantine.
    */
    FLAMEGPU_DEVICE_FUNCTION void exit_from_quarantine(DeviceAPI<MessageBucket, MessageNone> *FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning exit_from_quarantine for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
        const short extern_node = FLAMEGPU->environment.getProperty<unsigned short>(EXTERN_NODE);
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
        const int quarantine_node = FLAMEGPU->getVariable<int>(ROOM_FOR_QUARANTINE_INDEX);
        const unsigned short agent_with_a_rate = FLAMEGPU->getVariable<unsigned short>(AGENT_WITH_A_RATE);

        short solution_quarantine_extern[SOLUTION_LENGTH] = {-1};
        unsigned short target_index = FLAMEGPU->getVariable<unsigned short>(TARGET_INDEX);

        auto env_hours_schedule = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_HOURS_SCHEDULE);
        auto env_swab_distr = FLAMEGPU->environment.getMacroProperty<int>(ENV_SWAB_DISTR);
        auto env_swab_distr_firstparam = FLAMEGPU->environment.getMacroProperty<float>(ENV_SWAB_DISTR_FIRSTPARAM);
        auto env_swab_distr_secondparam = FLAMEGPU->environment.getMacroProperty<float>(ENV_SWAB_DISTR_SECONDPARAM);
        auto counters = FLAMEGPU->environment.getMacroProperty<unsigned int, NUM_COUNTERS>(COUNTERS);

        printf("[QUARANTINE],%d,%d,%d,%d,%d,1\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<unsigned short>(QUARANTINE) ? 1: 0, FLAMEGPU->getVariable<unsigned short>(SEVERITY));

        FLAMEGPU->setVariable<unsigned short>(SEVERITY, MINOR);
        FLAMEGPU->setVariable<int>(SWAB_STEPS, -1);
        FLAMEGPU->setVariable<int>(ROOM_FOR_QUARANTINE_INDEX, -1);
        FLAMEGPU->setVariable<unsigned short>(QUARANTINE, 0);
        FLAMEGPU->setVariable<unsigned short>(IDENTIFIED_INFECTED, NOT_IDENTIFIED);

        a_star(FLAMEGPU, quarantine_node, extern_node, solution_quarantine_extern);

        if(!agent_with_a_rate){
            if(quarantine_node != extern_node)
                update_targets(FLAMEGPU, solution_quarantine_extern, &target_index, false, 1);
            update_flow(FLAMEGPU, true);
        }
        else{
            unsigned short week_day = (FLAMEGPU->environment.getProperty<unsigned short>(WEEK_DAY) + 1) % DAYS_IN_A_WEEK;

            unsigned short empty_days = 0;
            unsigned short agent_index = week_day;
            while((int) env_hours_schedule[agent_type][agent_index][1] - (int) env_hours_schedule[agent_type][agent_index][0] == 0){
                empty_days++;
                agent_index = (agent_index + 1) % DAYS_IN_A_WEEK;
            }

            const unsigned short start_step = (unsigned short) cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_HOURS_SCHEDULE_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, (int) env_hours_schedule[agent_type][agent_index][0], (int) env_hours_schedule[agent_type][agent_index][1], true);

            int stay_spawnroom = (STEPS_IN_A_DAY - ((FLAMEGPU->getStepCounter() + START_STEP_TIME) % STEPS_IN_A_DAY)) + start_step;

            if(quarantine_node != extern_node)
                update_targets(FLAMEGPU, solution_quarantine_extern, &target_index, false, stay_spawnroom);
            else
                FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, stay_spawnroom);
        }

        if((int) env_swab_distr[agent_type] != NO_SWAB){
            int swab_steps = round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_SWAB_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], (int) env_swab_distr[agent_type], contacts_id, STEPS_IN_A_DAY * (float) env_swab_distr_firstparam[agent_type], STEPS_IN_A_DAY * (float) env_swab_distr_secondparam[agent_type], true));
            FLAMEGPU->setVariable<int>(SWAB_STEPS, swab_steps);
        }

        FLAMEGPU->setVariable<float, 3>(FINAL_TARGET, 0, FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDX, extern_node));
        FLAMEGPU->setVariable<float, 3>(FINAL_TARGET, 1, FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDY, extern_node));
        FLAMEGPU->setVariable<float, 3>(FINAL_TARGET, 2, FLAMEGPU->environment.getProperty<unsigned short, V>(INDEX2COORDZ, extern_node));

        float agent_pos[3] = {FLAMEGPU->getVariable<float>(X), FLAMEGPU->getVariable<float>(Y), FLAMEGPU->getVariable<float>(Z)};

        printf("[AGENT_POSITION_AND_STATUS],%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,0,1\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<int>(AGENT_TYPE), (int) agent_pos[0], (int) agent_pos[1], (int) agent_pos[2], FLAMEGPU->getVariable<int>(DISEASE_STATE), FLAMEGPU->getVariable<unsigned short>(WEEK_DAY_FLOW), FLAMEGPU->getVariable<unsigned short>(FLOW_INDEX), FLAMEGPU->getStepCounter() - FLAMEGPU->getVariable<unsigned int>(LAST_STEP_MOVE));
        FLAMEGPU->setVariable<unsigned int>(LAST_STEP_MOVE, FLAMEGPU->getStepCounter());

        if(quarantine_node != extern_node)
            FLAMEGPU->setVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE, 1);

        counters[AGENTS_IN_QUARANTINE]--;
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending exit_from_quarantine for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * Handle internal screening.
    */
    FLAMEGPU_DEVICE_FUNCTION void screening(DeviceAPI<MessageBucket, MessageNone> *FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning screening for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
        const unsigned short identified_bool = FLAMEGPU->getVariable<unsigned short>(IDENTIFIED_INFECTED);

        auto env_swab_distr = FLAMEGPU->environment.getMacroProperty<int>(ENV_SWAB_DISTR);
        auto env_quarantine_swab_days_distr = FLAMEGPU->environment.getMacroProperty<int>(ENV_QUARANTINE_SWAB_DAYS_DISTR);

        int swab_steps = FLAMEGPU->getVariable<int>(SWAB_STEPS);

        if(((int) env_swab_distr[agent_type] != NO_SWAB || (int) env_quarantine_swab_days_distr[agent_type] != NO_QUARANTINE_SWAB) && swab_steps != -1){
            if(swab_steps){
                swab_steps = swab_steps - 1;
                FLAMEGPU->setVariable<int>(SWAB_STEPS, swab_steps);
            }
            else{
                swab(FLAMEGPU);
            }
        }
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending screening for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * Handle external screening.
    */
    FLAMEGPU_DEVICE_FUNCTION void external_screening(DeviceAPI<MessageBucket, MessageNone> *FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning external_screening for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
        const unsigned int disease_state = FLAMEGPU->getVariable<unsigned int>(DISEASE_STATE);

        auto env_external_screening_first = FLAMEGPU->environment.getMacroProperty<float>(ENV_EXTERNAL_SCREENING_FIRST);
        auto env_external_screening_second = FLAMEGPU->environment.getMacroProperty<float>(ENV_EXTERNAL_SCREENING_SECOND);

        unsigned short identified_bool = FLAMEGPU->getVariable<unsigned short>(IDENTIFIED_INFECTED);

        if(identified_bool != IDENTIFIED){
            float random_external_screening_first = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);
            if(random_external_screening_first < (float) env_external_screening_first[agent_type]){
                swab(FLAMEGPU);
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending external_screening for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
                return;
            }

            float random_external_screening_second = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);
            if(disease_state == INFECTED && random_external_screening_second < (float) env_external_screening_second[agent_type]){
                swab(FLAMEGPU);
            }
        }
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending external_screening for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * Handle contagion processes.
    */
    FLAMEGPU_DEVICE_FUNCTION flamegpu::AGENT_STATUS contagion_processes(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning of contagion_processes for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);

        int disease_state = FLAMEGPU->getVariable<int>(DISEASE_STATE);
        unsigned short incubation_days = FLAMEGPU->getVariable<unsigned short>(INCUBATION_DAYS);
        unsigned short infection_days = FLAMEGPU->getVariable<unsigned short>(INFECTION_DAYS);
        unsigned short fatality_days = FLAMEGPU->getVariable<unsigned short>(FATALITY_DAYS);;
        unsigned short end_of_immunization_days = FLAMEGPU->getVariable<unsigned short>(END_OF_IMMUNIZATION_DAYS);

        if(disease_state == SUSCEPTIBLE){
            // Contagion through contact
            float contamination_risk = FLAMEGPU->environment.getProperty<float>(CONTAMINATION_RISK);

            const float contamination_risk_decreased_with_mask = FLAMEGPU->environment.getProperty<float>(CONTAMINATION_RISK_DECREASED_WITH_MASK);
            const float virus_variant_factor = FLAMEGPU->environment.getProperty<float>(VIRUS_VARIANT_FACTOR);
            const float area_around_agent = M_PI * (DIAMETER / 2) * (DIAMETER / 2);

            const int mask_type = FLAMEGPU->getVariable<int>(MASK_TYPE);

            contamination_risk = (mask_type != NO_MASK) ? contamination_risk * (1 - contamination_risk_decreased_with_mask): contamination_risk;

            const float infected_contacts_minutes = ((float) (FLAMEGPU->getVariable<unsigned int>(INFECTED_CONTACTS_STEPS) / (60 / STEP))) * virus_variant_factor;

            const float p_contact = contamination_risk * (infected_contacts_minutes / area_around_agent);

            const float random_contact = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);

            // Contagion through aerosol
            const float risk_const = FLAMEGPU->environment.getProperty<float>(RISK_CONST);
            const float quanta_inhaled = FLAMEGPU->getVariable<float>(QUANTA_INHALED);
            const float p_aerosol = 1 - exp(-(quanta_inhaled / risk_const));

            const float random_aerosol = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);

            // See if the agent get the virus
            if(random_contact < p_contact || random_aerosol < p_aerosol){
                auto num_seird = FLAMEGPU->environment.getMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);
                num_seird[SUSCEPTIBLE]--;

#ifdef INCUBATION
                num_seird[EXPOSED]++;

                disease_state = EXPOSED;

                incubation_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_INCUBATION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INCUBATION_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INCUBATION_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INCUBATION_DAYS, 2), false)));
#else
                num_seird[INFECTED]++;

                disease_state = INFECTED;
                infection_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_INFECTION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 2), false)));
#ifdef FATALITY
                fatality_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_FATALITY_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 2), false)));
#endif
#endif
            }
        }

        FLAMEGPU->setVariable<float>(QUANTA_INHALED, 0.0f);
        FLAMEGPU->setVariable<unsigned int>(INFECTED_CONTACTS_STEPS, 0);
        
        FLAMEGPU->setVariable<int>(DISEASE_STATE, disease_state);
        FLAMEGPU->setVariable<unsigned short>(INCUBATION_DAYS, incubation_days);
        FLAMEGPU->setVariable<unsigned short>(INFECTION_DAYS, infection_days);
        FLAMEGPU->setVariable<unsigned short>(FATALITY_DAYS, fatality_days);
        FLAMEGPU->setVariable<unsigned short>(END_OF_IMMUNIZATION_DAYS, end_of_immunization_days);

        return ALIVE;
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending contagion_processes for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

/** 
     * Update disease state.
    */
    FLAMEGPU_DEVICE_FUNCTION void update_infection(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning of update_infected for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);

        int disease_state = FLAMEGPU->getVariable<int>(DISEASE_STATE);
        unsigned short incubation_days = FLAMEGPU->getVariable<unsigned short>(INCUBATION_DAYS);
        unsigned short infection_days = FLAMEGPU->getVariable<unsigned short>(INFECTION_DAYS);
        unsigned short fatality_days = FLAMEGPU->getVariable<unsigned short>(FATALITY_DAYS);
        unsigned short end_of_immunization_days = FLAMEGPU->getVariable<unsigned short>(END_OF_IMMUNIZATION_DAYS);

#ifdef INCUBATION
        if(disease_state == EXPOSED){
            if(!incubation_days){
                auto num_seird = FLAMEGPU->environment.getMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);
                num_seird[EXPOSED]--;
                num_seird[INFECTED]++;

                disease_state = INFECTED;

                infection_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_INFECTION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 2), false)));
#ifdef FATALITY
                fatality_days =  (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_FATALITY_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 2), false)));
#endif
            }
            else{
                incubation_days--;
            }
        }
#endif

        if(disease_state == INFECTED){
#ifdef FATALITY
            if(!fatality_days){
                // TO DO: handle correctly the dead of an agent (i.e., release the resource of the room in which he is)
                auto num_seird = FLAMEGPU->environment.getMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);
                num_seird[INFECTED]--;
                num_seird[DIED]++;

                disease_state = DIED;
                return DEAD;
            }
            else{
                fatality_days--;
            }
#endif

            if(!infection_days){
                auto num_seird = FLAMEGPU->environment.getMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);
                num_seird[INFECTED]--;
                num_seird[RECOVERED]++;

                disease_state = RECOVERED;
#ifdef REINFECTION
                end_of_immunization_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_END_OF_IMMUNIZATION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_END_OF_IMMUNIZATION_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_END_OF_IMMUNIZATION_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_END_OF_IMMUNIZATION_DAYS, 2), false)));
#endif
            }
            else{
                infection_days--;
            }
        }

#ifdef REINFECTION
        if(disease_state == RECOVERED){
            if(!end_of_immunization_days){
                auto num_seird = FLAMEGPU->environment.getMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);
                num_seird[RECOVERED]--;
                num_seird[SUSCEPTIBLE]++;

                disease_state = SUSCEPTIBLE;
            }
            else{
                end_of_immunization_days--;
            }
        }
#endif
        
        FLAMEGPU->setVariable<int>(DISEASE_STATE, disease_state);
        FLAMEGPU->setVariable<unsigned short>(INCUBATION_DAYS, incubation_days);
        FLAMEGPU->setVariable<unsigned short>(INFECTION_DAYS, infection_days);
        FLAMEGPU->setVariable<unsigned short>(FATALITY_DAYS, fatality_days);
        FLAMEGPU->setVariable<unsigned short>(END_OF_IMMUNIZATION_DAYS, end_of_immunization_days);
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending update_infected for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * Handle outside contagion.
    */
    FLAMEGPU_DEVICE_FUNCTION void outside_contagion(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU){
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Beginning of outside_contagion for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        if(FLAMEGPU->getVariable<int>(DISEASE_STATE) == SUSCEPTIBLE){
            const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
            const unsigned short day = FLAMEGPU->environment.getProperty<unsigned short>(DAY)-1;
            const float perc_inf = FLAMEGPU->environment.getProperty<float, DAYS>(PERC_INF, day);

            float random = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);
            if(random < perc_inf){
                auto num_seird = FLAMEGPU->environment.getMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);
                auto counters = FLAMEGPU->environment.getMacroProperty<unsigned int, NUM_COUNTERS>(COUNTERS);

                num_seird[SUSCEPTIBLE]--;
                counters[NUM_INFECTED_OUTSIDE]++;

#ifdef INCUBATION
                num_seird[EXPOSED]++;
                FLAMEGPU->setVariable<int>(DISEASE_STATE, EXPOSED);

                unsigned short incubation_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_INCUBATION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INCUBATION_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INCUBATION_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INCUBATION_DAYS, 2), false)));

                FLAMEGPU->setVariable<unsigned short>(INCUBATION_DAYS, incubation_days);
#else
                num_seird[INFECTED]++;
                FLAMEGPU->setVariable<int>(DISEASE_STATE, INFECTED);

                unsigned short infection_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_INFECTION_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, 2), false)));
                FLAMEGPU->setVariable<unsigned short>(INFECTION_DAYS, infection_days);

#ifdef FATALITY
                unsigned short fatality_days = (unsigned short) max(0.0f, round(cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_FATALITY_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 0), contacts_id, FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 1), FLAMEGPU->environment.getProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, 2), false)));
                FLAMEGPU->setVariable<unsigned short>(FATALITY_DAYS, fatality_days);
#endif
#endif
            }
        }
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending outside_contagion for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    }

    /** 
     * Find the correct occurred event.
    */
    FLAMEGPU_DEVICE_FUNCTION unsigned char findLeftmostIndex(DeviceAPI<MessageBucket, MessageNone>* FLAMEGPU, int left, int right, const float target) {
        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);

        auto env_events_cdf = FLAMEGPU->environment.getMacroProperty<float, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_CDF);
        
        if(target > (float) env_events_cdf[agent_type][1])
            return left;

        int result = right;

        while (left <= right) {
            int mid = (int) (left + (right - left) / 2);

            if ((float) env_events_cdf[agent_type][mid] < target) {
                right = mid - 1;
            } else {
                left = mid + 1;
                result = mid; // Store the current index as a potential answer
            }
        }


        return result;
    }
}
#endif //_DEVICE_FUNCTIONS_CUH_