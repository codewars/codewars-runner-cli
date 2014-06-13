var util = require( 'util' ),
    spawn = require('child_process' ).spawn,
    config = require('./config' ),
    os = require('os' );

module.exports.start = function start(opts, cb, strategy) {
    cb = cb || function(){};

    if (opts.debug) console.info('Running ' + opts.language +'...');

    if (!opts.fixture) {
        run(opts, strategy.solutionOnly(), cb);
    }
    else {
        run(opts, strategy.fullProject(), cb);
    }
}

function run(opts, cmd, cb) {
    exec(opts, cmd.name, cmd.args, function(buffer) {
        reportBuffer(opts, buffer);
        cb(buffer);
    });
}

function exec(opts, name, args, cb) {
    function exit(reason) {
        if (!finished) {
            child.kill( 'SIGKILL' );
            buffer.stderr += reason + '\n';
            finished = true;
        }
    }

    var child = spawn(name, args ),
        buffer = {stdout: '', stderr: ''},
        start = new Date(),
        finished = false,
        maxTime = opts.timeout || config.timeouts[opts.language];

    // Listen
    child.stdout.on('data', function(data) {
        if (!!data) buffer.stdout += data;

        if (buffer.stdout.length > 300000) {
            buffer.status = 'max_buffer_reached'
            exit('Max Buffer reached: Too much information has been written to stdout.');
        }
    });

    child.stderr.on('data', function(data) {
        if (!!data) buffer.stderr += data;
    });

    child.on( 'exit', function( code ) {
        clearTimeout(timeout);
        finished = true;
        buffer.exitCode = code;
        buffer.wallTime = new Date() - start;
//        buffer.freemem = os.freemem();
        cb(buffer);
    });

    // prevent the process from running for too long
    var timeout = setTimeout(function() {
        if (!finished) {
            buffer.status = 'max_time_reached';
            exit('Process was terminated. It took longer than ' + maxTime + 'ms to complete');
        }
    }, maxTime);

    child.stdin.end();
};


function reportBuffer(opts, buffer) {
    if (opts.format == 'json') {
        console.log(JSON.stringify(buffer));
    }
    else {
        if (buffer.stdout) console.log( buffer.stdout );
        if (buffer.stderr) console.error( buffer.stderr );
        if (buffer.wallTime && opts.debug) {
            console.info( buffer.wallTime + 'ms' )
        }
    }
}
