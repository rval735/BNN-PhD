/*
 Copyright 2001 The University of Texas at Austin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
#ifndef _SPECIES_H_
#define _SPECIES_H_

#include "neat.h"
#include "speciesorganism.h"
#include "population.h"

namespace NEAT {

	// ---------------------------------------------  
	// SPECIES CLASS:
	//   A Species is a group of similar Organisms      
	//   Reproduction takes place mostly within a
	//   single species, so that compatible organisms
	//   can mate.                                      
	// ---------------------------------------------  
	class Species {
	public:
		int id;
		int age; //The age of the Species 
		real_t ave_fitness; //The average fitness of the Species
		real_t max_fitness; //Max fitness of the Species
		real_t max_fitness_ever; //The max it ever had
		size_t expected_offspring;
		bool novel;
		bool checked;
		bool obliterate;  //Allows killing off in competitive coevolution stagnation
		std::vector<SpeciesOrganism*> organisms; //The organisms in the Species
		int age_of_last_improvement;  //If this is too long ago, the Species will goes extinct
		real_t average_est; //When playing real-time allows estimating average fitness

		bool add_Organism(SpeciesOrganism *o);

		SpeciesOrganism *first();

		bool print_to_file(std::ostream &outFile);

		//Change the fitness of all the organisms in the species to possibly depend slightly on the age of the species
		//and then divide it by the size of the species so that the organisms in the species "share" the fitness
		void adjust_fitness();

		real_t compute_average_fitness(); 

		real_t compute_max_fitness();

		//Counts the number of offspring expected from all its members skim is for keeping track of remaining 
		// fractional parts of offspring and distributing them among species
		real_t count_offspring(real_t skim);

		//Compute generations since last improvement
		int last_improved() {
			return age-age_of_last_improvement;
		}

		//Remove an organism from Species
        void remove_eliminated();
        void remove_generation(int gen);

		real_t size() {
			return organisms.size();
		}

		//Perform mating and mutation to form next generation
        void reproduce(int ioffspring,
                       SpeciesOrganism &baby,
                       class GenomeManager *genome_manager,
                       std::vector<Species*> &sorted_species);

		// *** Real-time methods *** 

		Species(int i);

		//Allows the creation of a Species that won't age (a novel one)
		//This protects new Species from aging inside their first generation
		Species(int i,bool n);

		~Species();

	};

	// This is used for list sorting of Species by fitness of best organism highest fitness first 
	bool order_species(Species *x, Species *y);

	bool order_new_species(Species *x, Species *y);

}

#endif