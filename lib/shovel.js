var util = require('util'),
    spawn = require('child_process').spawn,
    config = require('./config'),
    os = require('os');

//runs the strategy specified in opts and reports the results of the run, then runs the callback
module.exports.start = function start(opts, cb, strategies)
{
    cb = cb || function (){};

    run(opts, strategies, function (buffer)
    {
        reportBuffer(opts, buffer);
        cb(buffer);
    });
};

//runs the strategy specified in opts.
//If the strategy succeeds, silently runs the callback
//If the stragegy fails, reports the results of the run then aborts
// TODO: This method and its usage within the java runner are very confusing. Clean it up, or possibly remove it.
module.exports.compile = function compile(opts, cb, strategies)
{
    cb = cb || function (){};

    run(opts, strategies, function (buffer)
    {
        if (buffer.exitCode == 0)
        {
            cb(buffer);
        }
        else
        {
            reportBuffer(opts, buffer);
        }
    });
}

// given the options provided and a list of strategies on how to handle them, run will pick the
// appropriate strategy and execute it.
function run(opts, strategies, cb)
{
    function runStrategy(strategy)
    {
        exec(opts, strategy.name, strategy.args, strategy.options, cb);
    }

    var strategy = (!opts.fixture) ? strategies.solutionOnly() : strategies.fullProject();

    // if a function is returned then we will want to call it with a callback. The callback expects the function
    // to invoke it with a passed in strategy. This is useful for when the specific strategy needs to first compile
    // some async code.
    if (typeof strategy == 'function')
    {
        strategy(function(strategy)
        {
            runStrategy(strategy)
        });
    }
    else
    {
        runStrategy(strategy)
    }
}

function exec(opts, name, args, process_options, cb)
{
    function exit(reason)
    {
        if (!finished)
        {
            child.kill('SIGKILL');
            buffer.stderr.push(reason + '\n');
            finished = true;
        }
        cb(buffer);
    }

    var child = spawn(name, args, process_options),
        buffer = {stdout: [], stderr: []},
        start = new Date(),
        finished = false,
        stdoutLength = 0,
        maxTime = opts.timeout || config.timeouts[opts.language];

    // Listen
    child.stdout.on('data', function (data)
    {
        if (!!data)
        {
            buffer.stdout.push(data);
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

    child.on('exit', function (code)
    {
        clearTimeout(timeout);
        finished = true;
        buffer.exitCode = code;
        buffer.wallTime = new Date() - start;
//        buffer.freemem = os.freemem();

        // cleanup temp files
        if (opts.tempFiles != null)
        {
            for (var i = 0, l = opts.tempFiles.length; i < l; i++)
            {
                fs.unlink(opts.tempFiles[i]);
            }
            delete opts.tempFiles;
        }
        cb(buffer);
        //ensure that node exits now, even if processes have forked off
        process.exit(0);
    });

    // prevent the process from running for too long
    var timeout = setTimeout(function ()
    {
        if (!finished)
        {
            buffer.status = 'max_time_reached';
            exit('Process was terminated. It took longer than ' + maxTime + 'ms to complete');
            //ensure that node exits now, even if processes have forked off
        }
        process.exit(1);
    }, maxTime);

    child.stdin.end();
};


function reportBuffer(opts, buffer)
{
    buffer.stdout = buffer.stdout.join('');
    buffer.stderr = buffer.stderr.join('');

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
            console.info(buffer.wallTime + 'ms')
        }
    }
}
