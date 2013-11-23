function Slide(input) {
	
	// var tempCommands = _.defaults(input, {
		// back : false,
		// down : false,
		// left : false,
		// right : false,
		// stackback : false,
		// up : false
	// });

	// tempCommands = _.pick(tempCommands, 'back', 'down', 'left', 'right', 'stackback', 'up');
	// if (_.any(tempCommands, notBoolean)) {
		// throw new Error('Invalid slide commands!');
	// }
// 
	// // Save valid commands
	// this.commands = tempCommands;
	
	this.updateCommands(input);
	
	// Save image as jQuery DOM object
	this.img = getDOMImagefromBase64(input.image, input.imagetype).hide();

	// Build own id at this point, this can be replaced later
	this.id = input.id;
}

$.extend(Slide.prototype, {
	getDomSlide : function() {
		return this.img.clone();
	},
	getCommands : function() {
		return _.clone(this.commands);
	},
	getId : function() {
		return this.id;
	},
	updateCommands : function(input) {
		console.log('updateCommands:')
		console.log(input);
		var tempCommands = _.defaults(input, {
			back : false,
			down : false,
			left : false,
			right : false,
			stackback : false,
			up : false
		});

		tempCommands = _.pick(tempCommands, 'back', 'down', 'left', 'right', 'stackback', 'up');
		if (_.any(tempCommands, notBoolean)) {
			throw new Error('Invalid slide commands!');
		}

		// Save valid commands
		this.commands = tempCommands;
	}
});

