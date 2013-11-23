// Gui controller
var gui = (function() {

	// State
	var state = {
		role : null,
		slideId : null
	};

	// Shortcuts to elements in the DOM
	var dom = {
		message : {
			greyscreen : null,
			container : null,
			headline : null,
			body : null
		},
		slide : null,
		commands : null,
		footer : null
	};

	/**
	 * 	Initialize the GUI handler.
	 *
	 *  @param {Object} An object containing a bunch of jQuery objects
	 */
	function init(elements) {
		dom.message.greyscreen = elements.message.greyscreen;
		dom.message.container = elements.message.container;
		dom.message.headline = elements.message.headline;
		dom.message.body = elements.message.body;
		dom.slide = elements.slide;
		dom.commands = elements.commands;
		dom.footer = elements.footer;

		console.log(dom);

		if (dom.slide == null || dom.commands == null || dom.footer == null || dom.message.greyscreen == null || dom.message.container == null || dom.message.headline == null || dom.message.body == null)
			throw new Error('GUI initialization failed!');
	}

	/**
	 * Shows an error message.
	 *
	 * @param {Object} errorHeadline The headline of the error message
	 * @param {Object} errorText The text of the error message
	 * @param {Object} callback The function to call, when the error message is closed
	 */
	function showErrorMessage(errorHeadline, errorText, callback) {
		// Test for appropiate arguments. Skip callback here, because it is optional.
		if ( typeof errorHeadline != 'string' || typeof errorText != 'string')
			throw new Error('At showErrorMessage: Inappropiate Arguments!');

		clearMessageContainer();

		// Set background color to green
		dom.message.container.addClass('errormessage');

		// Set content
		dom.message.headline.html(errorHeadline);
		dom.message.body.html(errorText);

		dom.message.greyscreen.show().click(function() {
			dom.message.greyscreen.fadeOut(500);
			if (callback != null && typeof callback == 'function') {
				callback();
			};
		});
	}

	/**
	 * Show a normal message to the user.
	 *
	 * @param {String} headline The headline of the error message.
	 * @param {String} text The text of the error message.
	 * @param {Function} callback A function to be called, when the message is confirmed
	 * @param {Object} argument A argument for the callback function.
	 */
	function showMessage(headline, text, callback, argument) {
		if ( typeof headline != 'string' || typeof text != 'string')
			throw new TypeError('At showErrorMessage: Inappropiate Arguments!');

		clearMessageContainer();

		dom.message.container.addClass('message')

		// Set content
		dom.message.headline.html(headline);
		dom.message.body.html(text);

		dom.message.greyscreen.show().click(function() {
			dom.message.greyscreen.fadeOut(500);
			if (callback != null && typeof callback == 'function') {
				callback(argument);
			};
		});
	}

	/**
	 * Clear the container for the message.
	 */
	function clearMessageContainer() {
		// Clear containers
		dom.message.container.removeClass();
		dom.message.container.addClass('container');
		dom.message.body.empty();
		dom.message.body.removeClass();
		dom.message.headline.empty();
		dom.message.headline.removeClass();
	}

	/**
	 * Show the correct link to the other role.
	 *
	 * @throws Exception is thrown if the role in the state is invalid.
	 */
	function assignRoleSelectorLink() {
		// Skip this, if there is only one role
		if (config.roles.length == 1)
			return;

		switch (state.role) {
			case 'speaker':
				$('.footer .speakerLink').hide();
				$('.footer .listenerLink').show();

				break;
			case 'listener':
				$('.footer .speakerLink').show();
				$('.footer .listenerLink').hide();

				break;
			default:
				throw new Error('There is an invalid role saved in the state!');
				break;
		}
	}

	/**
	 * Set new role in the state variable.
	 *
	 * @param {String} role The new role.
	 * @throws Exception is thrown if the role is not valid.
	 */
	function setRole(role) {
		if (_.contains(config.roles, role)) {
			state.role = role;
		} else {
			throw new Error('InValid role!');
		}
	}

	/**
	 * Shows a new slide in the gui.
	 *
	 * @param {Slide} slide Slide Slide to show.
	 */
	function placeNewSlide(slide) {
		if (!( slide instanceof Slide)) {
			throw new TypeError('Input is not of type Slide.');
		}

		// Clear old slide
		dom.slide.empty();
		var newSlide = slide.getDomSlide();
		dom.slide.append(newSlide);
		switch (state.role) {
			case 'speaker':
				dom.slide.removeClass('listener');
				dom.slide.addClass('speaker');

				// Built commands
				generateDOMCommands(slide.getCommands());
				dom.commands.show();

				break;
			case 'listener':
				dom.slide.addClass('listener');
				dom.slide.removeClass('speaker');

				dom.commands.hide();
				break;
			default:
				throw new TypeError('Invalid role specified!');
				break;
		}

		// Adjust the height of the slide.
		adjustSize();

		// Show the slide
		newSlide.show();

	}

	/**
	 * Generate the container for the DOM with the commands.
	 *
	 * @param {Object} com Object containing the commands
	 */
	function generateDOMCommands(com) {
		var domCom = null;
		$.each(com, function(index, elm) {
			domCom = dom.commands.find('.' + index);
			if (domCom.length == 0)
				throw new Error('Did not find command element in the DOM!');

			// console.log(domCom);
			if (elm === true) {
				domCom.find('.active').show();
				domCom.find('.inactive').hide();
				domCom.unbind('click');
				domCom.click(function() {
					remoteController.command.sendCommand(index);
				});
			} else {
				domCom.unbind('click');
				domCom.find('.active').hide();
				domCom.find('.inactive').show();
			}
		});

	}

	/**
	 * Get the current slide id
	 *
	 * @return {Object} slideId The id of the slide which is currently displayed.
	 */
	function getCurrentSlideId() {
		return state.slideId;
	}

	/**
	 * Set the slide id in the state variable.
	 *
	 * @param {Object} input
	 */
	function setCurrentSlideId(input) {
		state.slideId = input;
	}

	/**
	 * Reset the state of the gui.
	 * This is needed to restart the whole client application.
	 */
	function reset() {
		// Reset the state
		state = {
			role : null,
			slideId : null
		};

		// Clear the slide.
		dom.slide.empty();

		// Hide the commands section.
		dom.commands.hide();

		// Hide the mode links.
		dom.footer.find('.speakerLink, .listenerLink').hide();

		// Reset the DOM references
		dom = {
			message : {
				greyscreen : null,
				container : null,
				headline : null,
				body : null
			},
			slide : null,
			commands : null,
			footer : null
		};
	}

	/**
	 * Adjust the size of the slide.
	 * Can be triggered on the resize event of the window.
	 */
	function adjustSize() {
		if (null == state.role || null == dom.slide)
			throw new TypeError('Adjusting size is not possible!');

		// Get the current height of the document.
		var offset, docHeight = $(document).height(), docWidth = $(document).width();

		// Check, if we are on a big screen.
		var bigScreen = docHeight > 900 && docWidth > 700;

		// Determine the space to be reserverved.
		if (state.role == 'speaker') {
			offset = bigScreen ? 310 : 210;
		} else {
			offset = bigScreen ? 70 : 40;
		}

		// Set the new height of the slide.
		dom.slide.find('.image').css('max-height', docHeight - offset);
	}

	// Return the public functions
	return {
		init : init,
		showErrorMessage : showErrorMessage,
		setRole : setRole,
		showMessage : showMessage,
		getCurrentSlideId : getCurrentSlideId,
		placeNewSlide : placeNewSlide,
		setCurrentSlideId : setCurrentSlideId,
		assignRoleSelectorLink : assignRoleSelectorLink,
		reset : reset,
		adjustSize : adjustSize
	};
})();
