var fs = require( 'fs' ),
    config = require( './config' ),
    then = require( './util' ).then;

function processLanguage(opts, cb){

    switch (opts.language){
        case 'js':
        case 'javascript':
        case 'jscript':
            opts.language = 'javascript';
            opts.ext = 'js';
            break;

        case 'coffee':
        case 'coffeescript':
            opts.language = 'coffeescript';
            opts.ext = 'coffee';
            break;

        case 'rb':
        case 'ruby':
            opts.language = 'ruby';
            opts.ext = 'rb';
            break;

        case 'py':
        case 'python':
            opts.language = 'python';
            opts.ext = 'py';
            break;

        default:
            return cb('language is missing or not supported');
    }

    cb();
}

// checks one of the code options (setup, solution, fixture, etc),
// converts any inline code into a file, validates the argument if its required.
function processCodeOption(key, opts, required, cb){
    var file_key = key + 'File';

    if (required && !opts[key] && !opts[file_key]) {
        cb(key + ' or ' + file_key + ' must be specified');
    }
    // if a file was provided then we will read it in, as we will likely need to modify it later
    else if (opts[file_key]){
        fs.readFile(opts[file_key], 'utf-8', function(err, data) {
            if (err) {
                cb( err );
            }else if (!data) {
                cb( key + ' file was not found or has no data' );
            }else {
                opts[key] = data;
                cb();
            }
        });
    }
    else {
        cb();
    }
}

module.exports.process = function process(opts, cb){

    // make sure the tmp folder exists, in case we need it later.
    try { fs.mkdirSync('tmp'); } catch(e){}

    processLanguage(opts, function() {
        processCodeOption('setup', opts, false, then(cb, function() {
            processCodeOption('solution', opts, true, then(cb, function() {
                processCodeOption( 'fixture', opts, false, then(cb, function () {

                    // set the default test framework if it isn't already set
                    opts.testFramework = opts.testFramework || config.testFramework.defaults[opts.language];

                    if (opts.debug) console.info( 'Processed options: ', opts );
                    cb( opts );
                }));
            }));
        }));
    });
};


