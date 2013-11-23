$(document).ready(function() {
	// Build dummy implementation of console to avoid problems on browsers without console
	if ( typeof console == 'undefined')
		console = {
			log : $.noop,
			debug : $.noop,
			error : $.noop
		};

	/**
	 * Starting the local client.
	 * Calls itself if there is an error occured.
	 */
	function start() {
		try {
			// Initialize the gui
			gui.init({
				message : {
					greyscreen : $('.greyscreen'),
					container : $('.greyscreen .container'),
					headline : $('.greyscreen .container h1.message-headline'),
					body : $('.greyscreen .container div.message-body')
				},
				slide : $('.content .slide'),
				commands : $('.content .commands'),
				footer : $('.footer')
			});

			// Request the current role
			remoteController.role.requestCurrent(function(data) {
				// Check received data
				if (data == null || !data.hasOwnProperty('role') || !data.hasOwnProperty('possible') || !(data.possible instanceof Array) || typeof data.role != "string") {
					throw new TypeError('No or incorrect role informationen received!');
				} else {
					// Save available roles to config object
					config.roles = data.possible;

					// Check for validity
					if (_.contains(config.roles, data.role)) {
						gui.setRole(data.role);
					} else {
						throw new TypeError('Invalid role from server received!');
					}
				}

				// Show the corect link to switch the role
				gui.assignRoleSelectorLink();

				// Start the waiting loop
				remoteController.slide.waitForUpdate();
			});
		} catch(e) {
			// Post the exception to the client for debugging reasons
			console.error(e);
			console.log('Restarting client...');

			// Restart the client with a delay to avoid burning the CPU
			setTimeout(function() {
				// Restart client
				gui.reset();
				start();
			}, 1000);
		}
	};// End of function "start"

	// Call start function
	start();

	// Assign event handler for resizing the slide in a proper way
	$(window).resize(gui.adjustSize);

	// Scroll away address bar on iphone
	var mobileIdentifier = /mobi/i;
	mobileIdentifier.test(navigator.userAgent) && !location.hash && setTimeout(function() {
		if (!pageYOffset)
			window.scrollTo(0, 1);
	}, 1000);});
// End of $().ready()

/*****************************************************************************/
// Slides represents a storage of the slide
var slides = (function() {
	// Define private slide list
	var slides = {};
	var slideIDs = [];

	/**
	 * Get a slide out of the slide storage
	 *
	 * @param {Object} id The id of the slide requested.
	 * @return {Slide} The slide.
	 */
	function getSlide(id) {
		return slides[id] || false;
	}

	/**
	 * Store a new slide in the slide storage.
	 *
	 * @param {Object} data The raw data as it comes from the server
	 */
	function pushSlide(data) {
		// Cancel if slide is already stored
		if (getSlide(data.id) !== false)
			return;

		if (slideIDs.length >= config.slideCacheSize)
			delete slides[slideIDs.shift()];

		slides[data.id] = new Slide(data);
		slideIDs.push(data.id);

		// Create slide object and store it
		slides[data.id] = new Slide(data);
	}

	/**
	 * Return a list of all slide ids in the storage.
	 *
	 * @return {Array} List of slide ids.
	 */
	function getSlideIdList() {
		return _.clone(slideIDs);
	}

	return {
		pushSlide : pushSlide,
		getSlide : getSlide,
		getSlideIdList : getSlideIdList
	};
})();

/*****************************************************************************/
/* End of client.js */