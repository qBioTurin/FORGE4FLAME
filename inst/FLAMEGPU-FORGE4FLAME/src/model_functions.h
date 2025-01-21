#ifndef _MODEL_FUNCTIONS_H_
#define _MODEL_FUNCTIONS_H_

#include "flamegpu/flamegpu.h"

using namespace flamegpu;

// Define model's environment
void define_environment(ModelDescription&);

// Define model's agents pedestrian messages
void define_pedestrian_messages(ModelDescription&);

// Define model's agents room messages
void define_room_messages(ModelDescription&);

// Define model's agents pedestrian
void define_pedestrian(ModelDescription&);

// Define model's agents room
void define_room(ModelDescription&);

// Define model's execution layers
void define_layers(ModelDescription&);

// Define model's visualisation
#ifdef FLAMEGPU_VISUALISATION
void define_visualisation(visualiser::ModelVis&);
#endif

#endif //_MODEL_FUNCTIONS_H_
