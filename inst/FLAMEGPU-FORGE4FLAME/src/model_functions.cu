#include "defines.h"
#include "model_functions.h"
#include "agent_functions.cuh"
#include "host_functions.cuh"

using namespace std;
using namespace flamegpu;
using namespace host_functions;

// Define model's agents pedestrian functions
void define_pedestrian_functions(AgentDescription& pedestrian){
    AgentFunctionDescription CUDAInitContagionScreeningEventsAndMovePedestrian_fn = pedestrian.newFunction("CUDAInitContagionScreeningEventsAndMovePedestrian", CUDAInitContagionScreeningEventsAndMovePedestrian);
    CUDAInitContagionScreeningEventsAndMovePedestrian_fn.setMessageInput("room_location");
    CUDAInitContagionScreeningEventsAndMovePedestrian_fn.setAllowAgentDeath(true);
    
#ifndef CHECKPOINT
    AgentFunctionDescription outputPedestrianLocation_fn = pedestrian.newFunction("outputPedestrianLocation", outputPedestrianLocation);
    outputPedestrianLocation_fn.setFunctionCondition(initCondition);
    outputPedestrianLocation_fn.setMessageOutput("location");

    AgentFunctionDescription outputPedestrianLocationAerosol_fn = pedestrian.newFunction("outputPedestrianLocationAerosol", outputPedestrianLocationAerosol);
    outputPedestrianLocationAerosol_fn.setFunctionCondition(initCondition);
    outputPedestrianLocationAerosol_fn.setMessageOutput("aerosol_counting");

    AgentFunctionDescription updateQuantaInhaledAndContacts_fn = pedestrian.newFunction("updateQuantaInhaledAndContacts", updateQuantaInhaledAndContacts);
    updateQuantaInhaledAndContacts_fn.setFunctionCondition(initCondition);
    updateQuantaInhaledAndContacts_fn.setMessageInput("location");

    //ciò va nel checkpoint?
    AgentFunctionDescription waitingInWaitingRoom_fn = pedestrian.newFunction("waitingInWaitingRoom", waitingInWaitingRoom);
    waitingInWaitingRoom_fn.setMessageInput("queue_message");
    waitingInWaitingRoom_fn.setMessageOutput("waiting_room_message");
    waitingInWaitingRoom_fn.setMessageOutputOptional(true);


#endif
}

// Define model's agents room functions
void define_room_functions(AgentDescription& room, string room_type){
    if(room_type != FILLINGROOM_AGENT_STRING){
        AgentFunctionDescription outputRoomLocation_fn = room.newFunction("outputRoomLocation", outputRoomLocation);
        outputRoomLocation_fn.setFunctionCondition(notInitAndNotFillingroomCondition);
        outputRoomLocation_fn.setMessageOutput("room_location");
#ifndef CHECKPOINT
        AgentFunctionDescription updateQuantaConcentration_fn = room.newFunction("updateQuantaConcentration", updateQuantaConcentration);
        updateQuantaConcentration_fn.setFunctionCondition(initAndNotFillingroomCondition);
        updateQuantaConcentration_fn.setMessageInput("aerosol_counting");
#endif
        //again ciò va nel checkpoint?
        AgentFunctionDescription handlingQueueinWaitingRoom_fn = room.newFunction("handlingQueueinWaitingRoom", handlingQueueinWaitingRoom);
        handlingQueueinWaitingRoom_fn.setMessageInput("waiting_room_message");
        handlingQueueinWaitingRoom_fn.setMessageOutput("queue_message");
        handlingQueueinWaitingRoom_fn.setMessageOutputOptional(true);
    }
}

// Visualise model's agents pedestrian
#ifdef FLAMEGPU_VISUALISATION
void visualise_pedestrians(visualiser::ModelVis& vis){
    visualiser::iDiscreteColor cmap(DISEASE_STATE, visualiser::Stock::Colors::WHITE);
    cmap[SUSCEPTIBLE] = visualiser::Color("#00FF00");
#ifdef INCUBATION
    cmap[EXPOSED] = visualiser::Color("#0000FF");
#endif
    cmap[INFECTED] = visualiser::Color("#FF0000");
    cmap[RECOVERED] = visualiser::Color("#000000");

    // Pedestrian agents
    visualiser::AgentVis ped_agt = vis.addAgent("pedestrian");
    
    ped_agt.setKeyFrameModel(visualiser::Stock::Models::PEDESTRIAN, ANIMATE);
    
    ped_agt.setModelScale(1.0f);
    ped_agt.setForwardXVariable(VELX);
    ped_agt.setForwardZVariable(VELZ);
    ped_agt.setColor(cmap);
}

// Visualise model's agents room
void visualise_rooms(visualiser::ModelVis& vis){
    visualiser::Color rooms_colors[NUM_COLORS] = COLORS;
    string rooms_types[NUM_ROOMS_TYPES] = ROOMS;

    visualiser::iDiscreteColor cmap(COLOR_ID, visualiser::Stock::Colors::WHITE);
    for(int c = 0; c < NUM_COLORS; c++){
        cmap[c] = rooms_colors[c];
    }

    // Room agents
    for(string room_type: rooms_types){
        {
            visualiser::AgentVis room_agt = vis.addAgent(room_type);
            // Position vars are named x, y, z; so they are used by default
            if(room_type != FILLINGROOM_AGENT_STRING)
                room_agt.setModel("resources/f4f/obj/room.obj");
            else
                room_agt.setModel("resources/f4f/obj/fillingroom.obj");
            
            room_agt.setModelScale(1.0f, 1.0f, 1.0f);

            room_agt.setScaleXVariable(LENGTH_OBJ);
            room_agt.setScaleYVariable(HEIGHT_OBJ);
            room_agt.setScaleZVariable(WIDTH_OBJ);

            room_agt.setYawVariable(YAW);

            room_agt.setColor(cmap);
        }
    }
}
#endif


// Define model's environment
void define_environment(ModelDescription& model){
    // Global environment variables
    EnvironmentDescription env = model.Environment();

    // Environment properties
    env.newProperty<unsigned int>(SEED, 123456789);
    
    env.newProperty<unsigned short, V>(INDEX2COORDX, {0});
    env.newProperty<unsigned short, V>(INDEX2COORDY, {0});
    env.newProperty<unsigned short, V>(INDEX2COORDZ, {0});
    env.newProperty<short, V>(NODE_TYPE, {0});
    env.newProperty<float, V>(NODE_YAW, {0.0f});
    env.newProperty<int, V>(NODE_LENGTH, {0});
    env.newProperty<int, V>(NODE_WIDTH, {0});
    
    env.newProperty<float, 4>(EXTERN_RANGES, {0.0f}); // Eventually modify to handle different entrances

    env.newProperty<unsigned short>(EXTERN_NODE, 0); // Eventually modify to handle different entrances
    env.newProperty<short>(NEXT_CONTACTS_ID, 0);
    env.newProperty<unsigned short>(DAY, 1);
    env.newProperty<unsigned short>(WEEK_DAY, 0);
    env.newProperty<unsigned short, 3>(MEAN_INCUBATION_DAYS, {0});
    env.newProperty<unsigned short, 3>(MEAN_INFECTION_DAYS, {0});
    env.newProperty<unsigned short, 3>(MEAN_END_OF_IMMUNIZATION_DAYS, {0});
    env.newProperty<unsigned short, 3>(MEAN_FATALITY_DAYS, {0});

    env.newProperty<float, 3>(EXHALATION_MASK_EFFICACY, {0.0f});
    env.newProperty<float, 3>(INHALATION_MASK_EFFICACY, {0.0f});
    env.newProperty<float>(CONTAMINATION_RISK, 0.0f);
    env.newProperty<float>(CONTAMINATION_RISK_DECREASED_WITH_MASK, 0.0f);
    env.newProperty<float>(NGEN_BASE, 0.0f);
    env.newProperty<float>(VL, 0.0f);
    env.newProperty<float>(VIRUS_VARIANT_FACTOR, 0.0f);
    env.newProperty<float>(DECAY_RATE, 0.0f);
    env.newProperty<float>(GRAVITATIONAL_SETTLING_RATE, 0.0f);
    env.newProperty<float>(INHALATION_RATE_PURE, 0.0f);
    env.newProperty<float>(RISK_CONST, 0.0f);

    env.newProperty<float, DAYS>(PERC_INF, {0.0f});

    env.newProperty<float>(SENSITIVITY_SWAB, 0.0f);
    env.newProperty<float>(SPECIFICITY_SWAB, 0.0f);
    env.newProperty<float>(VIRUS_SEVERITY, 0.0f);

    env.newProperty<unsigned short>(RUN_IDX, 0);
    

    env.newMacroProperty<short, FLOORS, ENV_DIM_Z, ENV_DIM_X>(COORD2INDEX);

    env.newMacroProperty<unsigned short, V, V>(ADJMATRIX);

    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_AREA);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR_FIRSTPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_FLOW_DISTR_SECONDPARAM);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_ACTIVITY_TYPE);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_HOURS_SCHEDULE);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_BIRTH_RATE_DISTR);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_BIRTH_RATE_DISTR_FIRSTPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, DAYS_IN_A_WEEK, SOLUTION_LENGTH>(ENV_BIRTH_RATE_DISTR_SECONDPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_AREA);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_CDF);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_ACTIVITY_TYPE);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_DISTR);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_DISTR_FIRSTPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, SOLUTION_LENGTH>(ENV_EVENTS_DISTR_SECONDPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_MASK_TYPE);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_MASK_FRACTION);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_VACCINATION_FRACTION);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_VACCINATION_EFFICACY);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_SWAB_DISTR);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_SWAB_DISTR_FIRSTPARAM);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_SWAB_DISTR_SECONDPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_QUARANTINE_DAYS_DISTR);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_QUARANTINE_DAYS_DISTR_FIRSTPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_QUARANTINE_DAYS_DISTR_SECONDPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_QUARANTINE_SWAB_DAYS_DISTR);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_QUARANTINE_SWAB_DAYS_DISTR_FIRSTPARAM);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_QUARANTINE_SWAB_DAYS_DISTR_SECONDPARAM);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_ROOM_FOR_QUARANTINE_TYPE);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_ROOM_FOR_QUARANTINE_AREA);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_EXTERNAL_SCREENING_FIRST);
    env.newMacroProperty<float, NUMBER_OF_AGENTS_TYPES_PLUS_1>(ENV_EXTERNAL_SCREENING_SECOND);

    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(INITIAL_INFECTED);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES_PLUS_1>(NUMBER_OF_AGENTS_BY_TYPE);

    env.newMacroProperty<unsigned int, TOTAL_AGENTS_OVERESTIMATION, TOTAL_AGENTS_OVERESTIMATION>(NUMBER_OF_STEPS_CONTACTS);

    env.newMacroProperty<unsigned int, DISEASE_STATES>(COMPARTMENTAL_MODEL);

    env.newMacroProperty<float, V>(ROOMS_QUANTA_CONCENTRATION);

    env.newMacroProperty<int, V>(GLOBAL_RESOURCES);
    env.newMacroProperty<unsigned int, V>(GLOBAL_RESOURCES_COUNTER);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES);
    env.newMacroProperty<unsigned int, NUMBER_OF_AGENTS_TYPES, V>(SPECIFIC_RESOURCES_COUNTER);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(ALTERNATIVE_RESOURCES_AREA);
    env.newMacroProperty<int, NUMBER_OF_AGENTS_TYPES, V>(ALTERNATIVE_RESOURCES_TYPE);


    env.newMacroProperty<unsigned int, NUM_COUNTERS>(COUNTERS);

    env.newMacroProperty<unsigned int, TOTAL_AGENTS_OVERESTIMATION>(CUDA_RNG_OFFSETS_PEDESTRIAN);
    env.newMacroProperty<unsigned int, NUM_ROOMS>(CUDA_RNG_OFFSETS_ROOM);
}

// Define model's agents pedestrian messages
void define_pedestrian_messages(ModelDescription& model){
    // Location pedestrian message
    MessageSpatial3D::Description pedestrian_message = model.newMessage<MessageSpatial3D>("location");
    pedestrian_message.newVariable<id_t>(ID);
    pedestrian_message.newVariable<short>(CONTACTS_ID);
    pedestrian_message.newVariable<int>(DISEASE_STATE);
    pedestrian_message.newVariable<int>(AGENT_TYPE);
    pedestrian_message.newVariable<short>(GRAPH_NODE);
    pedestrian_message.setRadius(DIAMETER / 2);
    pedestrian_message.setMin(0, 0, 0);
    pedestrian_message.setMax(ENV_DIM_X, ENV_DIM_Y, ENV_DIM_Z);

    // Aerosol counting message
    MessageBucket::Description aerosol_message = model.newMessage<MessageBucket>("aerosol_counting");
    aerosol_message.newVariable<int>(DISEASE_STATE);
    aerosol_message.newVariable<int>(MASK_TYPE);
    aerosol_message.newVariable<float>(ACTIVITY_TYPE);
    aerosol_message.setBounds(-1, V);

    // Message for the waiting room
    MessageBucket::Description queue_message = model.newMessage<MessageBucket>("queue_message");
    queue_message.newVariable<short>(GRAPH_NODE);
    queue_message.setBounds(0, TOTAL_AGENTS_OVERESTIMATION);
    queue_message.setPersistent(true);

}

// Define model's agents room messages
void define_room_messages(ModelDescription& model){
    // Room location message
    MessageBucket::Description room_message = model.newMessage<MessageBucket>("room_location");
    room_message.newVariable<unsigned short>(X);
    room_message.newVariable<unsigned short>(Y);
    room_message.newVariable<unsigned short>(Z);
    room_message.newVariable<short>(GRAPH_NODE);
    room_message.newVariable<int>(AREA);
    room_message.setBounds(0, V);
    room_message.setPersistent(true);

    // Handle resources message
    MessageBucket::Description waiting_room_message = model.newMessage<MessageBucket>("waiting_room_message");
    waiting_room_message.newVariable<short>(CONTACTS_ID);
    waiting_room_message.newVariable<int>(WAITING_ROOM_TIME);
    waiting_room_message.newVariable<int>(AGENT_TYPE);
    waiting_room_message.setPersistent(true);
    waiting_room_message.setBounds(0, V); 


}

// Define model's agents pedestrian
void define_pedestrian(ModelDescription& model){
    // Pedestrian agents
    AgentDescription pedestrian = model.newAgent("pedestrian");

    pedestrian.newVariable<unsigned short>(CUDA_INITIALIZED);
    pedestrian.newVariable<float>(X);
    pedestrian.newVariable<float>(Y);
    pedestrian.newVariable<float>(Z);
    pedestrian.newVariable<float>(ANIMATE);
    pedestrian.newVariable<float>(VELX);
    pedestrian.newVariable<float>(VELY);
    pedestrian.newVariable<float>(VELZ);
    pedestrian.newVariable<float>(QUANTA_INHALED);
    pedestrian.newVariable<float, 3>(FINAL_TARGET);
    pedestrian.newVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_X);
    pedestrian.newVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Y);
    pedestrian.newVariable<float, SOLUTION_LENGTH>(INTERMEDIATE_TARGET_Z);
    pedestrian.newVariable<unsigned int, SOLUTION_LENGTH>(STAY);
    pedestrian.newVariable<unsigned short>(NEXT_INDEX);
    pedestrian.newVariable<unsigned short>(TARGET_INDEX);
    pedestrian.newVariable<unsigned short>(FLOW_INDEX);
    pedestrian.newVariable<unsigned short>(INCUBATION_DAYS);
    pedestrian.newVariable<unsigned short>(INFECTION_DAYS);
    pedestrian.newVariable<unsigned short>(FATALITY_DAYS);
    pedestrian.newVariable<unsigned short>(END_OF_IMMUNIZATION_DAYS);
    pedestrian.newVariable<unsigned char>(INIT);
    pedestrian.newVariable<unsigned int>(INFECTED_CONTACTS_STEPS);
    pedestrian.newVariable<short>(ANIMATE_DIR, 1);
    pedestrian.newVariable<short>(CONTACTS_ID, -1);
    pedestrian.newVariable<int>(DISEASE_STATE);
    pedestrian.newVariable<int>(MASK_TYPE, NO_MASK);
    pedestrian.newVariable<int>(ROOM_FOR_QUARANTINE_INDEX, -1);
    pedestrian.newVariable<int>(AGENT_TYPE, -1);
    pedestrian.newVariable<unsigned short>(AGENT_WITH_A_RATE, AGENT_WITHOUT_RATE);
    pedestrian.newVariable<unsigned short>(SEVERITY, MINOR);
    pedestrian.newVariable<unsigned short>(QUARANTINE);
    pedestrian.newVariable<unsigned short>(IDENTIFIED_INFECTED, NOT_IDENTIFIED);
    pedestrian.newVariable<int>(SWAB_STEPS, -1);
    pedestrian.newVariable<unsigned short>(ENTRY_TIME_INDEX);
    pedestrian.newVariable<unsigned short>(JUST_EXITED_FROM_QUARANTINE);
    pedestrian.newVariable<unsigned short>(WEEK_DAY_FLOW);
    pedestrian.newVariable<unsigned char>(IN_AN_EVENT);
    pedestrian.newVariable<unsigned int>(LAST_STEP_MOVE);
    pedestrian.newVariable<int>(WAITING_ROOM_TIME, 0);
    pedestrian.newVariable<int>(WAITING_ROOM_FLAG, 0);
    pedestrian.newVariable<int>(ENTRY_EXIT_FLAG, 0);
    pedestrian.newVariable<short>(NODE_WAITING_FOR, -1);

    define_pedestrian_functions(pedestrian);
}

// Define model's agents room
void define_room(ModelDescription& model){
    // Room agents
    string rooms_types[NUM_ROOMS_TYPES] = ROOMS;

    for(string room_type: rooms_types){
        {
            AgentDescription room = model.newAgent(room_type);
            room.newVariable<float>(X, 0.0f);
            room.newVariable<float>(Y, 0.0f);
            room.newVariable<float>(Z, 0.0f);
            room.newVariable<float>(LENGTH_OBJ, 0.0f);
            room.newVariable<float>(WIDTH_OBJ, 0.0f);
            room.newVariable<float>(HEIGHT_OBJ, 0.0f);
            room.newVariable<float>(YAW, 0.0f);
            room.newVariable<unsigned char>(INIT_ROOM, 0);
            room.newVariable<int>(AREA, -1);
            room.newVariable<int>(COLOR_ID, 0);
            if(room_type != FILLINGROOM_AGENT_STRING){
                room.newVariable<float>(VOLUME, 0.0f);
                room.newVariable<float>(VENTILATION, 0.0f);
                room.newVariable<float>(ROOM_QUANTA_CONCENTRATION, 0.0f);
                room.newVariable<unsigned short>(X_CENTER, 0);
                room.newVariable<unsigned short>(Y_CENTER, 0);
                room.newVariable<unsigned short>(Z_CENTER, 0);
            }

            define_room_functions(room, room_type);
        }
    }
}

void define_layers(ModelDescription& model){
    // Define the execution order
    // Layer 1
    {
        LayerDescription layer = model.newLayer();
        layer.addAgentFunction(CUDAInitContagionScreeningEventsAndMovePedestrian);
    }

    // Layer 2
    {
        LayerDescription layer = model.newLayer();
#ifndef CHECKPOINT
        layer.addAgentFunction(outputPedestrianLocationAerosol);
#endif
        layer.addAgentFunction("room", "outputRoomLocation");
    }

#ifndef CHECKPOINT
    // Layer 3
    {
        LayerDescription layer = model.newLayer();
        layer.addAgentFunction(outputPedestrianLocation);
        layer.addAgentFunction("room", "updateQuantaConcentration");
    }
    
    // Layer 4

    {
        LayerDescription layer = model.newLayer();
        layer.addAgentFunction(waitingInWaitingRoom);
    }

    // Layer 5

    {
        LayerDescription layer = model.newLayer();
        layer.addAgentFunction(updateQuantaInhaledAndContacts);
        layer.addAgentFunction("room", "handlingQueueinWaitingRoom");
    }
#endif

    //Define each host functions
    model.addInitFunction(initFunction);
    model.addInitFunction(macroPropertyIO);
    model.addInitFunction(generateAgents);
    model.addStepFunction(updateDay);
    model.addStepFunction(birth);
    model.addExitFunction(exitFunction);
}

// Define model's visualisation
#ifdef FLAMEGPU_VISUALISATION
void define_visualisation(visualiser::ModelVis& vis){
    vis.setWindowTitle("FLAME GPU 2 ABM");
    vis.setInitialCameraLocation(50, 50, 150);
    vis.setInitialCameraTarget(50, 0, 0);
    vis.setCameraSpeed(0.05f);
    vis.setBeginPaused(true);
    vis.setSimulationSpeed(10);
    vis.setClearColor(255, 255, 255);
    vis.setFPSColor(0, 0, 0);

    visualise_pedestrians(vis);
    visualise_rooms(vis);
    
    // Draw the discrete grid
    auto pen = vis.newLineSketch(0.0f, 0.0f, 0.0f, 0.5f); 
    for (unsigned char j = 0; j < FLOORS; j++){
        // Grid Lines
        for (unsigned short i = 0; i <= ENV_DIM_X; i += 1) {
            pen.addVertex(i, YOFFSET*j, 0);
            pen.addVertex(i, YOFFSET*j, ENV_DIM_Z);
        }

        for(unsigned short i = 0; i <= ENV_DIM_Z; i += 1) {
            pen.addVertex(0, YOFFSET*j, i);
            pen.addVertex(ENV_DIM_X, YOFFSET*j, i);
        }
    }
}
#endif
