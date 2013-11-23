var remoteController = {};

// Role section of the remoteController
remoteController.role = (function() {
	/**
	 * Request the current and possible roles from the server
	 */
	function requestCurrent(callback) {
		$.ajax({
			url : config.baseURL + '/role',
			cache : false,
			timeout : config.globalTimeOut,
			dataType : 'jsonp',
			success : callback,
			jsonp : 'jsonp_callback',
			error : function(jqXHR, textStatus, errorThrown) {
				throw new Error('No role information received:\nStatus: ' + textStatus + '\nStatus-Code: ' + jqXHR.status);
			}
		});
	}

	return {
		requestCurrent : requestCurrent
	};
})();

// Slide section of the remoteControler
remoteController.slide = (function() {

	/**
	 * Wait for an update from the server.
	 */
	function waitForUpdate() {
		var slideId = gui.getCurrentSlideId(),
		    urlId = slideId === null ? '' : 'id=' + slideId;
		
		var cache = slides.getSlideIdList();
		
		urlCache = cache.length == 0 ? '' : 'cache='+cache.join('|');
		
		$.ajax({
			url : config.baseURL + '/update?' + urlId + "&" + urlCache,
			cache : false,
			timeout : config.globalTimeOut,
			dataType : 'jsonp',
			jsonp : 'jsonp_callback',
			success : function(data) {
				if (data != null) {
					// Update, read ids
					var id = data.id;

					// Attempt to get slide from cache
					var slide = slides.getSlide(id);

					// If slide does not exist in local cache
					if (slide === false) {
						slides.pushSlide(data);
						slide = slides.getSlide(id);
					}
					else {
						slides.getSlide(id).updateCommands(data);
					}

					// Set ids in gui state object
					gui.setCurrentSlideId(id);

					// Place new slide in gui
					gui.placeNewSlide(slide);
				}

				// Restart the waiting function
				setTimeout(waitForUpdate, 500);
			},
			error : function(jqXHR, textStatus, errorThrown) {
				console.log(textStatus + ' ' + jqXHR.status);
				console.log(errorThrown);
				gui.showErrorMessage('Connection Error', 'An error occured, check your connection to the slide server.', function() {
					setTimeout(waitForUpdate, 2000);
				});
			}
		});

	}

	return {
		waitForUpdate : waitForUpdate
	}
})();

// Command section of the remoteController
remoteController.command = (function() {

	/**
	 * Send a command to the server.
	 *
	 * @param {String} command The command to execute.
	 * @throws Exception is thrown if the passed command is invalid.
	 *
	 */
	function sendCommand(command) {
		if (!(_.contains(config.commandTypes, command)))
			throw new Error('At sendCommand: Command is not valid!');

		// Send the command
		$.ajax({
			url : config.baseURL + '/command',
			cache : false,
			timeout : config.globalTimeOut,
			dataType : 'jsonp',
			jsonp : 'jsonp_callback',
			data : {
				'cmd' : command
			},
			success : function(data) {
				if (true == data) {
					console.log('Sent command successful!');
				} else {
					console.error('Command not permitted!');
				}
			},
			error : function(jqXHR, textStatus, errorThrown) {
				console.log(errorThrown);
				throw new Error(textStatus + ' ' + jqXHR.status);
			}
		});

	}

	return {
		sendCommand : sendCommand
	};
})();
