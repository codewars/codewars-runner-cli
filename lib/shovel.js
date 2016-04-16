var util = require('util'),
    spawn = require('child_process').spawn,
    config = require('./config'),
    os = require('os');

// used for indicating an error which is related to user code and not an application error.
// useful for when we run code compilation inside of this host process.
var CompileError = module.exports.CompileError = function(message) {
    this.name = "CompileError";
    this.message = message;
}
CompileError.prototype = Error.prototype;

//runs the strategy specified in opts and reports the results of the run, then runs the callback
module.exports.start = function start(opts, cb, strategies)
{
    cb = cb || function (){};
    if(strategies.compile)
    {
        module.exports.compile(opts, function()
        {
            run(opts, strategies, function (buffer)
            {
                reportBuffer(opts, buffer, strategies);
                cb(buffer);
            });
        }, strategies.compile);
    }
    else
    {
        run(opts, strategies, function (buffer)
        {
            reportBuffer(opts, buffer, strategies);
            cb(buffer);
        });
    }
};

// given the options provided and a list of strategies on how to handle them, run will pick the
// appropriate strategy and execute it.
function run(opts, strategies, cb)
{
    function runStrategy(strategy)
    {
        // if is error
        if (typeof strategy === 'string')
        {
            cb({stdout: [''], stderr: [strategy], wallTime: 0});
        }
        else
        {
            exec(opts, strategy.name, strategy.args, strategy.options, strategy.stdin, cb);
        }
    }

    try {
        (!opts.fixture) ? strategies.solutionOnly(runStrategy) : strategies.testIntegration(runStrategy);
    }
    catch(ex) {
        // our special error which allows us to raise an exception outside of a child process and still react to it as
        // if its normal.
        if (ex.name === "CompileError") {
            runStrategy(ex.message);
        }
        else throw ex;
    }
}

function exec(opts, name, args, process_options, process_stdin, cb)
{
    var args = args || [],
	      child = spawn(name, args, process_options),
        buffer = {stdout: [], stderr: []},
        start = new Date(),
        finished = false,
        stdoutLength = 0,
        maxTime = opts.timeout || config.timeouts[opts.language] || config.timeouts.default;

    function exit(reason)
    {
        if (!finished)
        {
            child.kill('SIGKILL');
            if (reason) buffer.stderr.push(reason + '\n');
            finished = true;
        }
        cb(buffer);
    }

    function cleanupFiles()
    {
        // cleanup temp files
        if (opts.tempFiles)
        {
            for (var i = 0, l = opts.tempFiles.length; i < l; i++)
            {
                fs.unlink(opts.tempFiles[i]);
            }
            delete opts.tempFiles;
        }
    }

    if (process_stdin) child.stdin.write(process_stdin);

    // Listen
    child.stdout.on('data', function (data)
    {
        if (!!data)
        {
            buffer.stdout.push(data.toString());
            stdoutLength += data.length;
        }

        if (stdoutLength > 750000)
        {
            buffer.status = 'max_buffer_reached';
            exit('Max Buffer reached: Too much information has been written to stdout.');
        }
    });

    child.stderr.on('data', function (data)
    {
        if (!!data) buffer.stderr.push(data.toString());
    });

    child.on('error', exit);

    // prevent the process from running for too long
    var timeout = setTimeout(function ()
    {
        if (!finished)
        {
            buffer.status = 'max_time_reached';
            exit('Process was terminated. It took longer than ' + maxTime + 'ms to complete');
        }
        process.exit(1);
    }, maxTime);

    child.on('exit', function (code)
    {
        clearTimeout(timeout);
        finished = true;
        buffer.exitCode = code;
        buffer.wallTime = new Date() - start;

        cleanupFiles();
        cb(buffer);

        // if we are within the run script
        if (!opts.compiling && (process.argv[1] || '').indexOf('/run') >= 0)
        {
            //ensure that node exits now, even if processes have forked off
            process.exit(0);
        }
    });


    child.stdin.end();
}


function reportBuffer(opts, buffer, strategies)
{
    buffer.stdout = buffer.stdout.join('');
    buffer.stderr = buffer.stderr.join('');

    if (strategies)
    {
        if(strategies.transformOutput)
        {
           buffer.stdout = strategies.transformOutput(opts);
        }

        // if there is an error, provide the ability to sanitize it. This is useful for when
        // output can be noisy.
        if (buffer.stderr && strategies.sanitizeStdErr)
        {
            buffer.stderr = strategies.sanitizeStdErr(buffer.stderr);
        }

        if (buffer.stdout && strategies.sanitizeStdOut)
        {
            buffer.stdout = strategies.sanitizeStdOut(buffer.stdout);
        }
    }

    if (opts.format == 'json')
    {
        console.log(JSON.stringify(buffer));
    }
    else
    {
        if (buffer.stdout) console.log(buffer.stdout);
        if (buffer.stderr) console.error(buffer.stderr);
        if (buffer.wallTime && opts.debug)
        {
            console.info(buffer.wallTime + 'ms');
        }
    }
}
