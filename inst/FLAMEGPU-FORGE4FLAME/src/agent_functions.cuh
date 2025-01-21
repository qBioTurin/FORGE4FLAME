#ifndef _AGENT_FUNCTIONS_CUH_
#define _AGENT_FUNCTIONS_CUH_

#include "defines.h"
#include "device_functions.cuh"

using namespace std;
using namespace flamegpu;
using namespace device_functions;


/** 
    initCondition

    Execute a function if the agent is enabled
*/
FLAMEGPU_AGENT_FUNCTION_CONDITION(initCondition) {
    return FLAMEGPU->getVariable<unsigned char>(INIT);
}




/** 
    CUDAInitContagionScreeningEventsAndMovePedestrian

    Condition: -

    CUDA RNGs initialization, contagion processes (aerosol, contacts, and outside contagion), screening (internal and external), events and agent movements.
*/
FLAMEGPU_AGENT_FUNCTION(CUDAInitContagionScreeningEventsAndMovePedestrian, MessageBucket, MessageNone) {
#ifdef DEBUG
    printf("[DEBUG],%d,%d,Beginning CUDAInitContagionScreeningEventsAndMovePedestrian for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    // CUDA initialization
    if(!FLAMEGPU->getVariable<unsigned short>(CUDA_INITIALIZED)){
        auto cuda_rng_offsets_pedestrian = FLAMEGPU->environment.getMacroProperty<unsigned int, TOTAL_AGENTS_OVERESTIMATION>(CUDA_RNG_OFFSETS_PEDESTRIAN);

        curand_init((unsigned long long) FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getVariable<short>(CONTACTS_ID)+1, (unsigned int) cuda_rng_offsets_pedestrian[FLAMEGPU->getVariable<short>(CONTACTS_ID)], &cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)][FLAMEGPU->getVariable<short>(CONTACTS_ID)]);
        FLAMEGPU->setVariable<unsigned short>(CUDA_INITIALIZED, 1);
    }
    





    // Contagion processes
#ifndef CHECKPOINT
    flamegpu::AGENT_STATUS state = ALIVE;

    if(!((FLAMEGPU->getStepCounter() + START_STEP_TIME + 1) % STEPS_IN_A_DAY)){
        // Contagion processes (contacts and aerosol)
        state = contagion_processes(FLAMEGPU);

        if(state == DEAD)
            return state;

        // Outside contagion
        outside_contagion(FLAMEGPU);

        // Update disease state
        update_infection(FLAMEGPU);

        // External screening
        external_screening(FLAMEGPU);
    }

    // Screening
    screening(FLAMEGPU);
#endif






    // Generate event
    const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
    const int room_for_quarantine_index = FLAMEGPU->getVariable<int>(ROOM_FOR_QUARANTINE_INDEX);
    const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
    const unsigned int last_step_move = FLAMEGPU->getVariable<unsigned int>(LAST_STEP_MOVE);
    const unsigned short flow_index = FLAMEGPU->getVariable<unsigned short>(FLOW_INDEX);    
    const unsigned short extern_node = FLAMEGPU->environment.getProperty<unsigned short>(EXTERN_NODE);
    const unsigned short quarantine = FLAMEGPU->getVariable<unsigned short>(QUARANTINE);
    const unsigned short agent_with_a_rate = FLAMEGPU->getVariable<unsigned short>(AGENT_WITH_A_RATE);
    const float final_target[3] = {FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 0), FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 1), FLAMEGPU->getVariable<float, 3>(FINAL_TARGET, 2)};

    short solution[SOLUTION_LENGTH] = {-1};
    unsigned short identified = FLAMEGPU->getVariable<unsigned short>(IDENTIFIED_INFECTED);
    unsigned short next_index = FLAMEGPU->getVariable<unsigned short>(NEXT_INDEX);
    unsigned short target_index = FLAMEGPU->getVariable<unsigned short>(TARGET_INDEX);
    unsigned short week_day_flow = FLAMEGPU->getVariable<unsigned short>(WEEK_DAY_FLOW);
    unsigned int stay = FLAMEGPU->getVariable<unsigned int, SOLUTION_LENGTH>(STAY, next_index);
    float agent_pos[3] = {FLAMEGPU->getVariable<float>(X), FLAMEGPU->getVariable<float>(Y), FLAMEGPU->getVariable<float>(Z)};
    float agent_pos_init[3] = {FLAMEGPU->getVariable<float>(X), FLAMEGPU->getVariable<float>(Y), FLAMEGPU->getVariable<float>(Z)};
    float intermediate_target[3] = {FLAMEGPU->getVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_X, next_index), FLAMEGPU->getVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Y, next_index), FLAMEGPU->getVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Z, next_index)};

    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);
    auto global_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, V>(GLOBAL_RESOURCES_COUNTER);
    auto specific_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES_COUNTER);
    auto counters = FLAMEGPU->environment.getMacroProperty<unsigned int, NUM_COUNTERS>(COUNTERS);
    
    if(FLAMEGPU->getVariable<unsigned char>(INIT) && !quarantine && !FLAMEGPU->getVariable<unsigned char>(IN_AN_EVENT)){
        float random = cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_UNIFORM_0_1_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], UNIFORM, contacts_id, 0, 1, false);

        auto env_events = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS);
        auto env_events_area = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_AREA);
        auto env_events_distr = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_DISTR);
        auto env_events_distr_firstparam = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_DISTR_FIRSTPARAM);
        auto env_events_distr_secondparam = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_DISTR_SECONDPARAM);

        unsigned int num_events = 0;
        while((int) env_events[agent_type][num_events] != -1)
            num_events++;

        unsigned short event = findLeftmostIndex(FLAMEGPU, 0, num_events+1, random);

        if(event){
            short event_node = -1;
            float min_separation = numeric_limits<float>::max();

            // Searching the nearest room related to the event
            for(const auto& message: FLAMEGPU->message_in((int) env_events[agent_type][event])) {
                const unsigned short near_agent_pos[3] = {message.getVariable<unsigned short>(X), message.getVariable<unsigned short>(Y), message.getVariable<unsigned short>(Z)};

                float separation = abs(near_agent_pos[0] - agent_pos[0]) + abs(near_agent_pos[1] - agent_pos[1]) + abs(near_agent_pos[2] - agent_pos[2]);
                if(separation < min_separation){
                    min_separation = separation;
                    event_node = message.getVariable<short>(GRAPH_NODE);
                }
            }

            short start_node;
            
            if(next_index != target_index)
                start_node = coord2index[(unsigned short)(intermediate_target[1]/YOFFSET)][(unsigned short)intermediate_target[2]][(unsigned short)intermediate_target[0]];
            else
                start_node = coord2index[(unsigned short)(final_target[1]/YOFFSET)][(unsigned short)final_target[2]][(unsigned short)final_target[0]];

            const short final_node = coord2index[(unsigned short)(final_target[1]/YOFFSET)][(unsigned short)final_target[2]][(unsigned short)final_target[0]];

            short solution_start_event[SOLUTION_LENGTH] = {-1};
            short solution_event_target[SOLUTION_LENGTH] = {-1};

            a_star(FLAMEGPU, start_node, event_node, solution_start_event);
            a_star(FLAMEGPU, event_node, final_node, solution_event_target);

            int event_distr = (int) env_events_distr[agent_type][event];
            int event_distr_firstparam = (int) env_events_distr_firstparam[agent_type][event];
            int event_distr_secondparam = (int) env_events_distr_secondparam[agent_type][event];

            unsigned int event_time_random = (unsigned int) cuda_pedestrian_rng(FLAMEGPU, PEDESTRIAN_EVENT_DISTR_IDX, cuda_pedestrian_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)], event_distr, contacts_id, event_distr_firstparam, event_distr_secondparam, true);
            unsigned int final_stay;
            if(event_time_random < FLAMEGPU->getVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index)){
                final_stay = FLAMEGPU->getVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index) - event_time_random;
            }
            else{
                event_time_random = FLAMEGPU->getVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index);
                final_stay = 1;
            }

            update_targets(FLAMEGPU, solution_start_event, &target_index, true, event_time_random);
            update_targets(FLAMEGPU, solution_event_target, &target_index, false, final_stay);

            FLAMEGPU->setVariable<unsigned char>(IN_AN_EVENT, event);

            printf("[AGENT_POSITION_AND_STATUS],%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<int>(AGENT_TYPE), (int) agent_pos[0], (int) agent_pos[1], (int) agent_pos[2], FLAMEGPU->getVariable<int>(DISEASE_STATE), week_day_flow, flow_index, FLAMEGPU->getStepCounter() - last_step_move, FLAMEGPU->getVariable<unsigned char>(IN_AN_EVENT), FLAMEGPU->getVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE));
            FLAMEGPU->setVariable<unsigned int>(LAST_STEP_MOVE, FLAMEGPU->getStepCounter());
            
#ifdef DEBUG
            printf("[DEBUG],%d,%d,Ending CUDAInitContagionScreeningEventsAndMovePedestrian for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
            return ALIVE;
        }
    }

    // Move pedestrian
    auto env_flow = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW);

    // Handle the agent exited from the environment
    if(FLAMEGPU->getVariable<unsigned char>(INIT) && coord2index[(unsigned short)(final_target[1]/YOFFSET)][(unsigned short)final_target[2]][(unsigned short)final_target[0]] == extern_node && next_index == target_index && ((int) env_flow[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH] == -1 || ((flow_index - 1) % SOLUTION_LENGTH > 0 && (int) env_flow[agent_type][week_day_flow][((flow_index - 1) % SOLUTION_LENGTH)] == SPAWNROOM) || room_for_quarantine_index == extern_node || FLAMEGPU->getVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE))){        
        //printf("[AGENT_POSITION_AND_STATUS],%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,0,%d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<int>(AGENT_TYPE), (int) agent_pos.x, (int) INVISIBLE_AGENT_Y, (int) agent_pos.z, FLAMEGPU->getVariable<int>(DISEASE_STATE), week_day_flow, flow_index, FLAMEGPU->getStepCounter() - last_step_move, quarantine > 0 ? 1: 0);
        FLAMEGPU->setVariable<unsigned int>(LAST_STEP_MOVE, FLAMEGPU->getStepCounter());

        if(agent_with_a_rate && (int) env_flow[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH] == -1){
            auto num_seird = FLAMEGPU->environment.getMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);

            num_seird[FLAMEGPU->getVariable<int>(DISEASE_STATE)]--;
            counters[COUNTERS_KILLED_AGENTS_WITH_RATE]++;

#ifdef DEBUG
            printf("[DEBUG],%d,%d,Ending CUDAInitContagionScreeningEventsAndMovePedestrian for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
            return DEAD;
        }

        FLAMEGPU->setVariable<unsigned char>(INIT, 0);
        FLAMEGPU->setVariable<float>(Y, INVISIBLE_AGENT_Y);
        agent_pos[1] = INVISIBLE_AGENT_Y;

        if(!agent_with_a_rate && ((int) env_flow[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH] == -1 || ((flow_index - 1) % SOLUTION_LENGTH > 0 && (int) env_flow[agent_type][week_day_flow][((flow_index - 1) % SOLUTION_LENGTH)] == SPAWNROOM)) && !FLAMEGPU->getVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE))
            update_flow(FLAMEGPU, false);

        FLAMEGPU->setVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE, 0);
#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending CUDAInitContagionScreeningEventsAndMovePedestrian for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        return ALIVE;
    }

    
    // Decrement stay and eventually take the next (or first) destination in the agent's flow
    if(stay){
        stay = stay - 1;
        FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, next_index, stay);

        if(!stay && next_index == target_index && quarantine){
            exit_from_quarantine(FLAMEGPU);
            return ALIVE;
        }

        if(next_index == target_index && FLAMEGPU->getVariable<unsigned char>(IN_AN_EVENT))
            FLAMEGPU->setVariable<unsigned char>(IN_AN_EVENT, 0);

        if(!stay){
            printf("[AGENT_POSITION_AND_STATUS],%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<int>(AGENT_TYPE), (int) agent_pos[0], (int) agent_pos[1], (int) agent_pos[2], FLAMEGPU->getVariable<int>(DISEASE_STATE), week_day_flow, flow_index, FLAMEGPU->getStepCounter() - last_step_move, FLAMEGPU->getVariable<unsigned char>(IN_AN_EVENT), FLAMEGPU->getVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE));
            FLAMEGPU->setVariable<unsigned int>(LAST_STEP_MOVE, FLAMEGPU->getStepCounter());
        }

        if(!stay && next_index == target_index && (int) env_flow[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH] != -1){
            if(!FLAMEGPU->getVariable<unsigned char>(INIT) && (int) env_flow[agent_type][week_day_flow][(flow_index + 1) % SOLUTION_LENGTH] != SPAWNROOM){
                FLAMEGPU->setVariable<unsigned char>(INIT, 1);
                FLAMEGPU->setVariable<float>(Y, YEXTERN);
                agent_pos[1] = YEXTERN;
            }

            int flow_stay = 1;
            
            const short start_node = coord2index[(unsigned short)(final_target[1]/YOFFSET)][(unsigned short)final_target[2]][(unsigned short)final_target[0]];
            const short final_node = take_new_destination_flow(FLAMEGPU, &flow_stay);
            const short start_node_type = FLAMEGPU->environment.getProperty<short, V>(NODE_TYPE, start_node);

            if(start_node != extern_node && start_node != final_node && start_node_type != WAITINGROOM){
                unsigned int get_global_resource = --global_resources_counter[start_node]; 
                unsigned int get_specific_resources = --specific_resources_counter[agent_type][start_node];
            }

            a_star(FLAMEGPU, start_node, final_node, solution);

            update_targets(FLAMEGPU, solution, &target_index, false, flow_stay);

            //printf("[AGENT_POSITION_AND_STATUS],%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,0,%d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<int>(AGENT_TYPE), (int) agent_pos.x, (int) agent_pos.y, (int) agent_pos.z, FLAMEGPU->getVariable<int>(DISEASE_STATE), week_day_flow, flow_index, FLAMEGPU->getStepCounter() - last_step_move, quarantine > 0 ? 1: 0);
            FLAMEGPU->setVariable<unsigned int>(LAST_STEP_MOVE, FLAMEGPU->getStepCounter());
        }

        printf("noncredo proprio qui\n");

#ifdef DEBUG
        printf("[DEBUG],%d,%d,Ending CUDAInitContagionScreeningEventsAndMovePedestrian for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
        return ALIVE;
    }

    // Move the agent
    float agent_vel[3] = {FLAMEGPU->getVariable<float>(VELX), FLAMEGPU->getVariable<float>(VELY), FLAMEGPU->getVariable<float>(VELZ)};
    
    float available_vel = STEP;
    float distance = sqrt(pow(intermediate_target[0] - agent_pos[0], 2) + pow(intermediate_target[1] - agent_pos[1], 2) + pow(intermediate_target[2] - agent_pos[2], 2));

    while(distance < available_vel){
        agent_pos[0] = intermediate_target[0];
        agent_pos[1] = intermediate_target[1];
        agent_pos[2] = intermediate_target[2];
        available_vel = available_vel - distance;

        next_index = (next_index + 1) % SOLUTION_LENGTH;
        FLAMEGPU->setVariable<unsigned short>(NEXT_INDEX, next_index);
        stay = FLAMEGPU->getVariable<unsigned int, SOLUTION_LENGTH>(STAY, next_index);

        if(next_index != target_index && !stay){
            intermediate_target[0] = FLAMEGPU->getVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_X, next_index);
            intermediate_target[1] = FLAMEGPU->getVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Y, next_index);
            intermediate_target[2] = FLAMEGPU->getVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Z, next_index);
            distance = sqrt(pow(intermediate_target[0] - agent_pos[0], 2) + pow(intermediate_target[1] - agent_pos[1], 2) + pow(intermediate_target[2] - agent_pos[2], 2));
        }
        else
            available_vel = 0.0f;
    }
    
    // Update velocity
    agent_vel[0] = (available_vel * (intermediate_target[0] - agent_pos[0]))/std::max(1.0f, distance);
    agent_vel[1] = (available_vel * (intermediate_target[1] - agent_pos[1]))/std::max(1.0f, distance);
    agent_vel[2] = (available_vel * (intermediate_target[2] - agent_pos[2]))/std::max(1.0f, distance);

    // Update position
    agent_pos[0] += agent_vel[0];
    agent_pos[1] += agent_vel[1];
    agent_pos[2] += agent_vel[2];

    // Update animation
    if((agent_vel[0] != 0.0f || agent_vel[2] != 0.0f) && agent_vel[1] == 0.0f){
        const float agent_animate = FLAMEGPU->getVariable<float>(ANIMATE) + (FLAMEGPU->getVariable<short>(ANIMATE_DIR));
        if (agent_animate >= 1)
            FLAMEGPU->setVariable<short>(ANIMATE_DIR, -1);
        else if (agent_animate <= 0)
            FLAMEGPU->setVariable<short>(ANIMATE_DIR, 1);
        FLAMEGPU->setVariable<float>(ANIMATE, agent_animate);
    }

    // Update variables
    FLAMEGPU->setVariable<float>(X, agent_pos[0]);
    FLAMEGPU->setVariable<float>(Y, agent_pos[1]);
    FLAMEGPU->setVariable<float>(Z, agent_pos[2]);
    FLAMEGPU->setVariable<float>(VELX, agent_vel[0]);
    FLAMEGPU->setVariable<float>(VELY, agent_vel[1]);
    FLAMEGPU->setVariable<float>(VELZ, agent_vel[2]);

    if(agent_pos[0] != agent_pos_init[0] || agent_pos[1] != agent_pos_init[1] || agent_pos[2] != agent_pos_init[2]){
        printf("[AGENT_POSITION_AND_STATUS],%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<int>(AGENT_TYPE), (int) agent_pos[0], (int) agent_pos[1], (int) agent_pos[2], FLAMEGPU->getVariable<int>(DISEASE_STATE), week_day_flow, flow_index, FLAMEGPU->getStepCounter() - last_step_move, FLAMEGPU->getVariable<unsigned char>(IN_AN_EVENT), FLAMEGPU->getVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE));
        FLAMEGPU->setVariable<unsigned int>(LAST_STEP_MOVE, FLAMEGPU->getStepCounter());
    }

    printf("sarebbe un update da animazione\n");
#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending CUDAInitContagionScreeningEventsAndMovePedestrian for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif

    return ALIVE;
}


/** 
    outputPedestrianLocation

    Condition: initCondition

    Each pedestrian agent output a MessageSpatial3D message for counting contacts
*/
FLAMEGPU_AGENT_FUNCTION(outputPedestrianLocation, MessageNone, MessageSpatial3D) {

    printf("ma qui non arriva\n");
#ifdef DEBUG
    printf("[DEBUG],%d,%d,Beginning outputPedestrianLocation for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);

    const float agent_pos[3] = {FLAMEGPU->getVariable<float>(X), FLAMEGPU->getVariable<float>(Y), FLAMEGPU->getVariable<float>(Z)};
    const short node = coord2index[(unsigned short)(agent_pos[1]/YOFFSET)][(unsigned short)agent_pos[2]][(unsigned short)agent_pos[0]];

    FLAMEGPU->message_out.setVariable<id_t>(ID, FLAMEGPU->getID());
    FLAMEGPU->message_out.setVariable<short>(CONTACTS_ID, FLAMEGPU->getVariable<short>(CONTACTS_ID));
    FLAMEGPU->message_out.setVariable<int>(DISEASE_STATE, FLAMEGPU->getVariable<int>(DISEASE_STATE));
    FLAMEGPU->message_out.setVariable<int>(AGENT_TYPE, FLAMEGPU->getVariable<int>(AGENT_TYPE));
    FLAMEGPU->message_out.setVariable<short>(GRAPH_NODE, node);
    FLAMEGPU->message_out.setLocation(
        FLAMEGPU->getVariable<float>(X),
        FLAMEGPU->getVariable<float>(Y),
        FLAMEGPU->getVariable<float>(Z)
    );

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending outputPedestrianLocation for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    return ALIVE;
}


/** 
    outputPedestrianLocationAerosol

    Condition: initCondition

    Each pedestrian agent output a MessageBucket message for counting how many agents there are in a room (for aerosol transmission)
*/
FLAMEGPU_AGENT_FUNCTION(outputPedestrianLocationAerosol, MessageNone, MessageBucket) {

    printf("e qinvece qui entri e il problema è un altro?\n");
#ifdef DEBUG
    printf("[DEBUG],%d,%d,Beginning outputPedestrianLocationAerosol for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);
    auto env_activity_type = FLAMEGPU->environment.getMacroProperty<float, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_ACTIVITY_TYPE);
    auto env_events_activity_type = FLAMEGPU->environment.getMacroProperty<float, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_ACTIVITY_TYPE);

    //qua sarà da considerare anche se è nella waiting room. Di certo non è attività pesante
    const float agent_pos[3] = {FLAMEGPU->getVariable<float>(X), FLAMEGPU->getVariable<float>(Y), FLAMEGPU->getVariable<float>(Z)};
    const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
    const short node = coord2index[(unsigned short)(agent_pos[1]/YOFFSET)][(unsigned short)agent_pos[2]][(unsigned short)agent_pos[0]];
    const unsigned short flow_index = FLAMEGPU->getVariable<unsigned short>(FLOW_INDEX);
    const unsigned short quarantine = FLAMEGPU->getVariable<unsigned short>(QUARANTINE);
    const unsigned short week_day_flow = FLAMEGPU->getVariable<unsigned short>(WEEK_DAY_FLOW);
    const unsigned char in_an_event = FLAMEGPU->getVariable<unsigned char>(IN_AN_EVENT);
    
    float activity_type = VERY_LIGHT_ACTIVITY;
    if(!quarantine && flow_index > 0){
        activity_type = (float) env_activity_type[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH];

        if(in_an_event)
            activity_type = (float) env_events_activity_type[agent_type][in_an_event];
    }

    // TEMPORARY!!
    // const unsigned short day = FLAMEGPU->environment.getProperty<unsigned short>(DAY);

    FLAMEGPU->message_out.setVariable<int>(DISEASE_STATE, FLAMEGPU->getVariable<int>(DISEASE_STATE));
    // FLAMEGPU->message_out.setVariable<int>(MASK_TYPE, day >= 4 ? FLAMEGPU->getVariable<int>(MASK_TYPE): NO_MASK); // TEMPORARY!!
    FLAMEGPU->message_out.setVariable<int>(MASK_TYPE, FLAMEGPU->getVariable<int>(MASK_TYPE));
    FLAMEGPU->message_out.setVariable<float>(ACTIVITY_TYPE, activity_type);

    FLAMEGPU->message_out.setKey(node);

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending outputPedestrianLocationAerosol for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    return ALIVE;
}


/** 
    initAndNotFillingroomCondition

    Execute a function if the room is enabled and if the room is not a fillingroom
*/
FLAMEGPU_AGENT_FUNCTION_CONDITION(initAndNotFillingroomCondition) {
    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);

    unsigned short room_pos[3] = {FLAMEGPU->getVariable<unsigned short>(X_CENTER), FLAMEGPU->getVariable<unsigned short>(Y_CENTER), FLAMEGPU->getVariable<unsigned short>(Z_CENTER)};

    const short node = coord2index[(unsigned short)(room_pos[1]/YOFFSET)][(unsigned short)room_pos[2]][(unsigned short)room_pos[0]];
    const short node_type = FLAMEGPU->environment.getProperty<short, V>(NODE_TYPE, node);

    return FLAMEGPU->getVariable<unsigned char>(INIT_ROOM) && node_type != FILLINGROOM;
}

/** 
    updateQuantaConcentration

    Condition: initAndNotFillingroomCondition

    Use the number of agents inside a room to update the quanta concentration of that room (for aerosol transmission)
*/
FLAMEGPU_AGENT_FUNCTION(updateQuantaConcentration, MessageBucket, MessageNone) {
#ifdef DEBUG
    printf("[DEBUG],%d,%d,Beginning updateQuantaConcentration for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getID());
#endif
    unsigned short room_pos[3] = {FLAMEGPU->getVariable<unsigned short>(X_CENTER), FLAMEGPU->getVariable<unsigned short>(Y_CENTER), FLAMEGPU->getVariable<unsigned short>(Z_CENTER)};
    float room_quanta_concentration = FLAMEGPU->getVariable<float>(ROOM_QUANTA_CONCENTRATION);

    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);
    auto rooms_quanta_concentration = FLAMEGPU->environment.getMacroProperty<float, V>(ROOMS_QUANTA_CONCENTRATION);
    
    const short node = coord2index[(unsigned short)(room_pos[1]/YOFFSET)][(unsigned short)room_pos[2]][(unsigned short)room_pos[0]];
    const short node_type = FLAMEGPU->environment.getProperty<short, V>(NODE_TYPE, node);
    const float volume = FLAMEGPU->getVariable<float>(VOLUME);
    const float ventilation = FLAMEGPU->getVariable<float>(VENTILATION);
    const float vl = FLAMEGPU->environment.getProperty<float>(VL);
    const float ngen_base = FLAMEGPU->environment.getProperty<float>(NGEN_BASE);
    const float virus_variant_factor = FLAMEGPU->environment.getProperty<float>(VIRUS_VARIANT_FACTOR);
    const float gravitational_settling_rate = FLAMEGPU->environment.getProperty<float>(GRAVITATIONAL_SETTLING_RATE);
    const float decay_rate = FLAMEGPU->environment.getProperty<float>(DECAY_RATE);

    float total_n_r = 0.0f;
    for(const auto& message: FLAMEGPU->message_in(node)) {
        if(message.getVariable<int>(DISEASE_STATE) == INFECTED && node_type != CORRIDOR && node_type != DOOR){
            const float activity_type = message.getVariable<float>(ACTIVITY_TYPE);

            // TEMPORARY!!
            // const unsigned short day = FLAMEGPU->environment.getProperty<unsigned short>(DAY);
    
            // float exhalation_mask_efficacy = FLAMEGPU->environment.getProperty<float, 3>(EXHALATION_MASK_EFFICACY, day >= 4 ? message.getVariable<int>(MASK_TYPE): NO_MASK); // TEMPORARY!!
            float exhalation_mask_efficacy = FLAMEGPU->environment.getProperty<float, 3>(EXHALATION_MASK_EFFICACY, message.getVariable<int>(MASK_TYPE));
            float base_n_r = ((activity_type * ngen_base) / pow(10, 9)) * virus_variant_factor;

            total_n_r += (base_n_r * pow(10, vl)) * (1 - exhalation_mask_efficacy);
        }
    }

    float total_first_order_lost_rate = ventilation + gravitational_settling_rate + decay_rate;
    float new_concentration = ((total_n_r / volume) / total_first_order_lost_rate) + (((float) rooms_quanta_concentration[node]) - ((total_n_r / volume) / total_first_order_lost_rate)) * exp(-(total_first_order_lost_rate * STEP));

    rooms_quanta_concentration[node].exchange(new_concentration);
    FLAMEGPU->setVariable<float>(ROOM_QUANTA_CONCENTRATION, new_concentration);

    if(!((FLAMEGPU->getStepCounter() + START_STEP_TIME) % STEPS_IN_A_HOUR)){
        printf("[AEROSOL],%d,%d,%d,%d,%d,%f,%d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), (int) room_pos[0], (int) room_pos[1], (int) room_pos[2], (float) new_concentration, node);
    }

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending updateQuantaConcentration for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getID());
#endif
    return ALIVE;
}


/** 
    notInitAndNotFillingroomCondition

    Execute a function if the room is not enabled and if the room is not a fillingroom
*/
FLAMEGPU_AGENT_FUNCTION_CONDITION(notInitAndNotFillingroomCondition) {
    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);

    unsigned short room_pos[3] = {FLAMEGPU->getVariable<unsigned short>(X_CENTER), FLAMEGPU->getVariable<unsigned short>(Y_CENTER), FLAMEGPU->getVariable<unsigned short>(Z_CENTER)};
    
    const short node = coord2index[(unsigned short)(room_pos[1]/YOFFSET)][(unsigned short)room_pos[2]][(unsigned short)room_pos[0]];
    const short node_type = FLAMEGPU->environment.getProperty<short, V>(NODE_TYPE, node);

    return !FLAMEGPU->getVariable<unsigned char>(INIT_ROOM) && node_type != FILLINGROOM;
}

/** 
    outputRoomLocation

    Condition: notInitAndNotFillingroomCondition

    Each room agent output a MessageBucket message with its position(for handling events)
*/
FLAMEGPU_AGENT_FUNCTION(outputRoomLocation, MessageNone, MessageBucket) {

 printf("non è che in outputroom?\n");
#ifdef DEBUG
    printf("[DEBUG],%d,%d,Begin outputRoomLocation for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getID());
#endif
    // Initialize curand
    auto cuda_rng_offsets_room = FLAMEGPU->environment.getMacroProperty<unsigned int, NUM_ROOMS>(CUDA_RNG_OFFSETS_ROOM);
    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);

    curand_init(FLAMEGPU->environment.getProperty<unsigned int>(SEED), TOTAL_AGENTS_OVERESTIMATION+FLAMEGPU->getID(), cuda_rng_offsets_room[FLAMEGPU->getID()-1], &cuda_room_states[FLAMEGPU->environment.getProperty<unsigned short>(RUN_IDX)][FLAMEGPU->getID()-1]);

    unsigned short room_pos[3] = {FLAMEGPU->getVariable<unsigned short>(X_CENTER), FLAMEGPU->getVariable<unsigned short>(Y_CENTER), FLAMEGPU->getVariable<unsigned short>(Z_CENTER)};

    const short node = (short) coord2index[(unsigned short)(room_pos[1]/YOFFSET)][(unsigned short)room_pos[2]][(unsigned short)room_pos[0]];

    FLAMEGPU->message_out.setVariable<unsigned short>(X, room_pos[0]);
    FLAMEGPU->message_out.setVariable<unsigned short>(Y, room_pos[1]);
    FLAMEGPU->message_out.setVariable<unsigned short>(Z, room_pos[2]);
    FLAMEGPU->message_out.setVariable<short>(GRAPH_NODE, node);
    FLAMEGPU->message_out.setVariable<int>(AREA, FLAMEGPU->getVariable<int>(AREA));

    FLAMEGPU->message_out.setKey(FLAMEGPU->environment.getProperty<short, V>(NODE_TYPE, node));

    FLAMEGPU->setVariable<unsigned char>(INIT_ROOM, 1);

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending outputRoomLocation for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getID());
#endif
    return ALIVE;
}


/** 
    updateQuantaInhaledAndContacts

    Condition: initCondition
    Depends on: outputRoomLocation

    Use the quanta concentration in the room to update the quanta inhaled by the agent (for aerosol transmission) and count contacts
*/
FLAMEGPU_AGENT_FUNCTION(updateQuantaInhaledAndContacts, MessageSpatial3D, MessageNone) {
#ifdef DEBUG
    printf("[DEBUG],%d,%d,Beginning updateQuantaInhaledAndContacts for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    // Update quanta inhaled
    auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);

    const float agent_pos[3] = {FLAMEGPU->getVariable<float>(X), FLAMEGPU->getVariable<float>(Y), FLAMEGPU->getVariable<float>(Z)};
    const short node = coord2index[(unsigned short)(agent_pos[1]/YOFFSET)][(unsigned short)agent_pos[2]][(unsigned short)agent_pos[0]];

    if(FLAMEGPU->getVariable<int>(DISEASE_STATE) == SUSCEPTIBLE){
        auto rooms_quanta_concentration = FLAMEGPU->environment.getMacroProperty<float, V>(ROOMS_QUANTA_CONCENTRATION);
        auto env_activity_type = FLAMEGPU->environment.getMacroProperty<float, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_ACTIVITY_TYPE);
        auto env_events_activity_type = FLAMEGPU->environment.getMacroProperty<float, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_ACTIVITY_TYPE);

        const int agent_type = FLAMEGPU->getVariable<int>(AGENT_TYPE);
        const int mask_type = FLAMEGPU->getVariable<int>(MASK_TYPE);
        const float inhalation_rate_pure = FLAMEGPU->environment.getProperty<float>(INHALATION_RATE_PURE);
        const float inhalation_mask_efficacy = FLAMEGPU->environment.getProperty<float, 3>(INHALATION_MASK_EFFICACY, mask_type);
        const unsigned short flow_index = FLAMEGPU->getVariable<unsigned short>(FLOW_INDEX);
        const unsigned short quarantine = FLAMEGPU->getVariable<unsigned short>(QUARANTINE);
        const unsigned short week_day_flow = FLAMEGPU->getVariable<unsigned short>(WEEK_DAY_FLOW);
        const unsigned char in_an_event = FLAMEGPU->getVariable<unsigned char>(IN_AN_EVENT);
        
        float activity_type = VERY_LIGHT_ACTIVITY;
        if(!quarantine && flow_index > 0){
            activity_type = (float) env_activity_type[agent_type][week_day_flow][flow_index % SOLUTION_LENGTH];

            if(in_an_event)
                activity_type = (float) env_events_activity_type[agent_type][in_an_event];
        }
        
        float inhalation_rate = (inhalation_rate_pure * (1 - inhalation_mask_efficacy) * activity_type) / 1000;
        float concentration = (float) (node != -1) ? rooms_quanta_concentration[node]: 0.0f;

        float quanta_inhaled = FLAMEGPU->getVariable<float>(QUANTA_INHALED);
        quanta_inhaled = quanta_inhaled + inhalation_rate * STEP * concentration;
        FLAMEGPU->setVariable<float>(QUANTA_INHALED, quanta_inhaled);
    }






    // Update contacts
    const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);

    for (const auto& message: FLAMEGPU->message_in(agent_pos[0], agent_pos[1], agent_pos[2])) {
        const float near_agent_pos[3] = {message.getVariable<float>(X), message.getVariable<float>(Y), message.getVariable<float>(Z)};
        const short message_contacts_id = message.getVariable<short>(CONTACTS_ID);

        float x_diff = near_agent_pos[0] - agent_pos[0];
        float y_diff = near_agent_pos[1] - agent_pos[1];
        float z_diff = near_agent_pos[2] - agent_pos[2];
        const float separation = sqrt(x_diff*x_diff + y_diff*y_diff + z_diff*z_diff);

        if (separation > 0.0f && separation < (DIAMETER / 2) && node == message.getVariable<short>(GRAPH_NODE)){
            // Count contact inside the MacroProperty (each step)
            auto number_of_steps_contacts = FLAMEGPU->environment.getMacroProperty<unsigned int, TOTAL_AGENTS_OVERESTIMATION, TOTAL_AGENTS_OVERESTIMATION>(NUMBER_OF_STEPS_CONTACTS);
            number_of_steps_contacts[contacts_id][message_contacts_id]++;

            // Count contacts among a susceptible agent and an infected one.
            if(FLAMEGPU->getVariable<int>(DISEASE_STATE) == SUSCEPTIBLE && message.getVariable<int>(DISEASE_STATE) == INFECTED){
                unsigned int infected_contacts_steps = FLAMEGPU->getVariable<unsigned int>(INFECTED_CONTACTS_STEPS);
                FLAMEGPU->setVariable<unsigned int>(INFECTED_CONTACTS_STEPS, infected_contacts_steps + 1);

                printf("[CONTACT],%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID), message.getVariable<short>(CONTACTS_ID), FLAMEGPU->getVariable<int>(AGENT_TYPE), message.getVariable<int>(AGENT_TYPE), FLAMEGPU->getVariable<int>(DISEASE_STATE), message.getVariable<int>(DISEASE_STATE), (int) agent_pos[0], (int) agent_pos[1], (int) agent_pos[2]);
            }
        }
    }

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending updateQuantaInhaledAndContacts for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif
    return ALIVE;
}

/** 
    waitingInWaitingRoom

    Condition: //
    Depends on: //

    When the agent is waiting inside a waiting room he checks if the room has notificated that it's free and he can go.
    Otherwise, he waits.
*/

FLAMEGPU_AGENT_FUNCTION(waitingInWaitingRoom, MessageBucket, MessageBucket) {

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Beginning waitingInWaitingRoom for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif

    if(FLAMEGPU->getVariable<int>(WAITING_ROOM_FLAG) == INSIDE_WAITING_ROOM){

        const short contacts_id = FLAMEGPU->getVariable<short>(CONTACTS_ID);
        
        //if the agent is not already exiting from waiting room
        if(FLAMEGPU->getVariable<int>(ENTRY_EXIT_FLAG) == STAYING_IN_WAITING_ROOM){

            bool free = false;

            for(const auto& message: FLAMEGPU->message_in(contacts_id)){

                free = true;  
                break;
            }

            //If the room he's waiting for it's not free
            if(!free) {

                int time_waiting = FLAMEGPU->getVariable<int>(WAITING_ROOM_TIME);
                ++time_waiting;
                FLAMEGPU->setVariable<int>(WAITING_ROOM_TIME, time_waiting);
                unsigned short target_index = FLAMEGPU->getVariable<unsigned short>(TARGET_INDEX);
                FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, 2);

                FLAMEGPU->message_out.setVariable<short>(CONTACTS_ID, FLAMEGPU->getVariable<short>(CONTACTS_ID));
                FLAMEGPU->message_out.setVariable<int>(AGENT_TYPE, FLAMEGPU->getVariable<int>(AGENT_TYPE));
                FLAMEGPU->message_out.setVariable<int>(WAITING_ROOM_TIME, time_waiting);

                FLAMEGPU->message_out.setKey(FLAMEGPU->getVariable<short>(NODE_WAITING_FOR));

            }
            //If the room he's waiting for it's free
            else {
                unsigned short flow_index = FLAMEGPU->getVariable<unsigned short>(FLOW_INDEX) - 1;                        
                FLAMEGPU->setVariable<unsigned short>(FLOW_INDEX, flow_index);
                FLAMEGPU->setVariable<int>(ENTRY_EXIT_FLAG, EXITING_FROM_WAITING_ROOM);
                unsigned short target_index = FLAMEGPU->getVariable<unsigned short>(TARGET_INDEX);
                FLAMEGPU->setVariable<unsigned int, SOLUTION_LENGTH>(STAY, target_index, 1);
            }
        }
    }

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending waitingInWaitingRoom for agent with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getVariable<short>(CONTACTS_ID));
#endif

}


/** 
    handlingQueueinWaitingRoom

    Condition: //
    Depends on: //

    The room monitors the agent waiting inside it, if it's a waiting room. If it's free notifies it otherwise waits
*/
FLAMEGPU_AGENT_FUNCTION(handlingQueueinWaitingRoom, MessageBucket, MessageBucket) {

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Beginning handlingQueueInWaitingRoom for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getID());
#endif

        unsigned short room_pos[3] = {FLAMEGPU->getVariable<unsigned short>(X_CENTER), FLAMEGPU->getVariable<unsigned short>(Y_CENTER), FLAMEGPU->getVariable<unsigned short>(Z_CENTER)};
        auto coord2index = FLAMEGPU->environment.getMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);
        auto global_resources = FLAMEGPU->environment.getMacroProperty<int, V>(GLOBAL_RESOURCES);
        auto global_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, V>(GLOBAL_RESOURCES_COUNTER);
        auto specific_resources = FLAMEGPU->environment.getMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES);
        auto specific_resources_counter = FLAMEGPU->environment.getMacroProperty<unsigned int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES_COUNTER);
        const short node = coord2index[(unsigned short)(room_pos[1]/YOFFSET)][(unsigned short)room_pos[2]][(unsigned short)room_pos[0]];

        for(const auto& message: FLAMEGPU->message_in(node)){

            int agent_type = message.getVariable<int>(AGENT_TYPE);
            unsigned int get_global_resource = ++global_resources_counter[node];
            unsigned int get_specific_resources = ++specific_resources_counter[agent_type][node];

            if(get_global_resource <= global_resources[node] && get_specific_resources <= specific_resources[agent_type][node]){

                int max_time_waiting = INT_MIN;
                short contacts_id = -1;

                for(const auto& message: FLAMEGPU->message_in(node)){
                    
                    if(message.getVariable<int>(WAITING_ROOM_TIME) > max_time_waiting && message.getVariable<int>(AGENT_TYPE) == agent_type){
                      max_time_waiting = message.getVariable<int>(WAITING_ROOM_TIME);  
                      contacts_id = message.getVariable<short>(CONTACTS_ID);
                    } 
                }

                FLAMEGPU->message_out.setVariable<short>(GRAPH_NODE, node);
                FLAMEGPU->message_out.setKey(contacts_id); 

                break;
            }
            else {
                get_global_resource = --global_resources_counter[node];
                get_specific_resources = --specific_resources_counter[agent_type][node];
            }
        }

#ifdef DEBUG
    printf("[DEBUG],%d,%d,Ending handlingQueueInWaitingRoom for room with id %d\n", FLAMEGPU->environment.getProperty<unsigned int>(SEED), FLAMEGPU->getStepCounter(), FLAMEGPU->getID());
#endif

}



#endif //_AGENT_FUNCTIONS_CUH_