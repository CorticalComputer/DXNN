DXNN v1.0 Erlang,
By Gene Sher
http://DXNN.org
http://DXNNResearch.com
CorticalComputer@gmail.com

Brief documentation for this package is included in this README file.  

-------------
1. LICENSE
-------------

Copyright (C) 2009 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
All rights reserved.

This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

---------------------
2. USAGE and SUPPORT
---------------------

I hope that this software will be a useful starting point for your own
explorations in the creation of Computational Intelligence. The software is provided 
as is; however, I will do my best to maintain it and accommodate
suggestions. If you want to be notified of future releases of the
software or have questions, comments, bug reports or suggestions, send
an email to CorticalComputer@gmail.com, or request a user account at www.DXNNResearch.com

Alternatively, you may post your questions on dxnn.org or dxnnresearch.com

The following explains how to use DXNN.

INTRO
-----
DXNN is a fully distributed Topology and Weight Evolving Artificial Neural Network system created and invented by Gene Sher. 
Originaly introduced in the publication preprint of 2010 available on arXiv: arXiv: arXiv:1011.6022v3
The current state of the project is composed of the following:

Cortex controlling Neurons, Sensors, and Actuators. A population monitor that contorls a set of species, with the species being a morphological or another type of subset of the population. The organisms are the NNs, also reffered to as DXNNs. The DXNN platform also contains a system called Polis, which is a monitor of the various simulated environments (Scapes), where the NNs can evolve in a way of artificial life. Each scape represents a different environment. 

STARTING POINT
--------------
From inside Erlang.
1. %%%%Compilation%%%%
	make:all().
2. %%%%Initialize All Databases%%%%
	polis:init().
3. %%%%Start The Polis Databases%%%%
	polis:start().
4. At this point you can summon DXNNs or populations of DXNNs, construct Sensors and Actuators and provide them to the NNs... This section will be expaned in future additions, with the to be release in the future book containing detailed instructions and explanations on the creation and use of DXNN and General Topology and Weight Evolving Artificial Neural Networks (TWEANNs).

To set the population and agents to the preffered sensors and actuators, modify the INIT_CONSTRAINTS to use the appropriate settings, and then execute population_monitor:start(), which will create the population of size decided by you of agents using the specified sensors or actuators. If you want the agents to discover and explore the available sensors and actuators (perform feature selection in a sense) then modify modular_constructor, ensuring that you use the get_InitSensors() and get_InitActuators() function, instead of the get_Sensors() and get_Actuators() function used within the module. The get_Init.. starts the population off with the NN based agents using just a single sensor and actuator, exploring other avaiable sensors and actuators within the morphology as they evolve. you can add new sensors and actuators by specifying those sensors and actuators in the morphology module, and then creating those functions in the sensors and actuators modules. New mutation operators, activation functions... all can be added within the records.hrl, as long as those functions are realized in their respective modules so that they can be executed when called upon.

DXNN V2 will use the same algorithms, but will have a different architecture, more scalable, more versatile, simpler, better. It is currently in the works, nearing completion. It is because I'm currently working on this new version that this version was not released much sooner.
