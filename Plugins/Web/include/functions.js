var notBoolean = function(prop) {
	return typeof prop !== 'boolean';
};

var base64Matcher = new RegExp("^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$");

/**
 * @return jQuery Object
 */
function getDOMImagefromBase64(imagestring, type) {
	if(type == undefined){
		throw new Error('Image type undefined!');
	}
	if (base64Matcher.test(imagestring)) {
		var mimetype = type == 'jpg' ? 'jpeg' : type;
		return $('<img />').attr('src', 'data:image/' + mimetype + ';base64,' + imagestring).addClass('image');
	} else {
		throw new Error('Invalid image string!');
	}
}

/**
 * Convert boolean to zero or one.
 *
 * @return String
 */
function booleanToBinString(bool) {
	return bool ? '1' : '0';
}

/**
 * Build id for slide from raw data
 * As soon as native id is passed, this function just returns the id.
 *
 * @param String rawData
 */
function createSlideId(rawData) {
	return _.map([rawData.back, rawData.down, rawData.left, rawData.right, rawData.stackback, rawData.up], booleanToBinString).join('') + rawData.image.length;
}

