var util = require( 'util' ),
    spawn = require('child_process' ).spawn,
    config = require('./config' ),
    os = require('os');

//runs the strategy specified in opts and reports the results of the run, then runs the callback
module.exports.start = function start(opts, cb, strategy) {
    cb = cb || function(){};

    run(opts, strategy, function(buffer)
    {
        reportBuffer(opts, buffer);
        cb(buffer);
    });
}

//runs the strategy specified in opts.
//If the strategy succeeds, silently runs the callback
//If the stragegy fails, reports the results of the run then aborts
module.exports.compile = function compile(opts, cb, strategy) {
    cb = cb || function(){};

    run(opts, strategy, function(buffer)
    {
        if(buffer.exitCode == 0)
        {
            cb(buffer);
        }
        else
        {
            reportBuffer(opts, buffer);
        }
    });
}

function run(opts, strategy, cb) {
    var thisStrategy = (!opts.fixture)? strategy.solutionOnly() : strategy.fullProject();
    exec(opts, thisStrategy.name, thisStrategy.args, cb);
}

function exec(opts, name, args, cb) {
    function exit(reason) {
        if (!finished) {
            child.kill( 'SIGKILL' );
            buffer.stderr.push(reason + '\n');
            finished = true;
        }
    }

    var child = spawn(name, args),
        buffer = {stdout: [], stderr: []},
        start = new Date(),
        finished = false,
        stdoutLength = 0,
        maxTime = opts.timeout || config.timeouts[opts.language];

    // Listen
    child.stdout.on('data', function(data) {
        if (!!data) {
            buffer.stdout.push(data);
            stdoutLength += data.length;
        }

        if (stdoutLength > 400000) {
            buffer.status = 'max_buffer_reached';
            exit('Max Buffer reached: Too much information has been written to stdout.');
        }
    });

    child.stderr.on('data', function(data) {
        if (!!data) buffer.stderr.push(data.toString());
    });

    child.on( 'exit', function( code ) {
        clearTimeout(timeout);
        finished = true;
        buffer.exitCode = code;
        buffer.wallTime = new Date() - start;
//        buffer.freemem = os.freemem();
        cb(buffer);
        //ensure that node exits now, even if processes have forked off
        process.exit(1);
    });

    // prevent the process from running for too long
    var timeout = setTimeout(function() {
        if (!finished) {
            buffer.status = 'max_time_reached';
            exit('Process was terminated. It took longer than ' + maxTime + 'ms to complete');
            //ensure that node exits now, even if processes have forked off
            process.exit(1);
        }
    }, maxTime);

    child.stdin.end();
};


function reportBuffer(opts, buffer) {
    buffer.stdout = buffer.stdout.join('');
    buffer.stderr = buffer.stderr.join('');

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
