Readme file for the source code of the web client for HeuRIKA.

Files
- interface.html
	In this file you find the basic interface definition. It is just HTML.

- client.js
	This is the central file. It initializes the connection and the GUI. 
	After initialization, the control is handed over to the remoteController.
	Besides this file contains the local storage for the slides.

- include/gui.controller.js
	The guiController contains all methods to control the GUI.

- include/remote.controller.js
	The remoteController is divided into to parts (slide and role) and 
	handles all connections to the server.
	
- include/Slide.js
	Slide represents one single slide. Each new slide is an instance
	of this function/ class.
	
- include/functions.js
	Here you find several helper functions.
	
- include/config.js
	Here is the configuration located, e. g. the URL of the server.
	
- lib/*
	This directory contains all third party librarys used in this project.