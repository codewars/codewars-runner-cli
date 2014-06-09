var fs = require( 'fs' );

// async control flow helper method: returns a function that will either call the exit function or the next function,
// depending on the presence of an error.
module.exports.then = function then(exit, next) {
    return function(err) {
        if (err) {
            console.error(err);
            exit();
        }else {
            next();
        }
    };
}

// inline code gets saved to a file.
module.exports.writeSrc = function writeSrc(opts, key, cb) {
    var file = 'tmp/' + key + '.' + opts.ext;

    // write the temporary file
    fs.writeFile( file, opts[key], function (err) {
        if (err) {
            cb(err);
        }
        else {
            opts[key + 'Src'] = file;
            cb();
        }
    } );
};

module.exports.parseBodyToArgs = function parseBodyToArgs(body) {
    return  Object.keys(body ).reduce(function(args, value) {
        var prefix = value.length < 3 ? '-' : '--';
        return args + prefix + value + ' ' + body[value] + ' ';
    }, '');
}


