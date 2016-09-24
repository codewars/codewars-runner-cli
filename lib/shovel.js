var util = require('util'),
    spawn = require('child_process').spawn,
    config = require('./config'),
    os = require('os'),
    escapeHtml = require('escape-html'),
    codeWriteSync = require('./util').codeWriteSync,
    temp = require('temp'),
    services = require('./services'),
    Promise = require('bluebird');

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
    run(opts, strategies, function (buffer)
    {
        reportBuffer(opts, buffer, strategies);
        cb(buffer);
    });
};

// given the options provided and a list of strategies on how to handle them, run will pick the
// appropriate strategy and execute it.
function run(opts, strategies, cb)
{
    function executer(params)
    {
        // if is error
        if (typeof params === 'string')
        {
            cb({stdout: [''], stderr: [params], wallTime: 0});
        }
        else if (params.stdout || params.stderr) {
            cb(params);
        }
        else
        {
            exec(opts, params.name, params.args, params.options, params.stdin, cb);
        }
    }
    
    // write the solution to a text file so that it can be inspected if need be
    codeWriteSync(null, opts.code || opts.solution, '/home/codewarrior/', 'solution.txt', true);
    
    services.start(opts.services, function() {
        runShell(opts, function() {
            try {
                (!opts.fixture) ? strategies.solutionOnly(executer) : strategies.testIntegration(executer);
            }
            catch(ex) {
                executer(ex.stack || ex.toString());
            }
        });
    });
}

// handles running an optional shell value, which allows users to configure a shell script to be ran
// before the code executes
function runShell(opts, resolve) {

    // if a shell script is provided then run it now
    if (opts.shell) {
        temp.track();
        var file = codeWriteSync('bash', `#!/bin/bash\n${opts.shell}`, temp.mkdirSync('bash'), 'shell.sh'),
            shellOpts = {
                timeout: 5000, // allow the shell script its own 5 sec timeout
                compiling: true // set to true so that the script doesn't exit
            };

        exec(shellOpts, 'bash', [file], {}, null, function(result) {
            opts.shellResult = result;
            resolve();
        });
    }
    else {
        resolve();
    }
}

function exec(opts, name, args, processOptions, processStdin, cb)
{
    var args = args || [],
	      child = spawn(name, args, processOptions),
        buffer = {stdout: [], stderr: []},
        start = new Date(),
        finished = false,
        stdoutLength = 0,
        maxTime = opts.timeout || config.timeouts[opts.language] || config.timeouts.default,
        ably, channel = { publish: function(){} }

    if (opts.ably && opts.channel) {
        try {
            ably = new require('ably').Rest(opts.ably);
            channel = ably.channels.get(opts.channel);
        } catch(e)
        {}
    }

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

    if (processStdin) child.stdin.write(processStdin);

    // Listen
    child.stdout.on('data', function (data)
    {
        if (!!data)
        {
            var text = data.toString();
            buffer.stdout.push(text);
            stdoutLength += text.length;
            channel.publish('stdout', text);
        }

        if (stdoutLength > 1000000)
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

    var complete = function(code) {
        if(finished) return;

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
    }

    child.on('exit', function (code) {
        // wait the remaining time left to see if all stdio processes close
        // preferably we cleanup after 'exit' is called
        setTimeout(function() {
            complete(code);
        }, getTimeLeft(timeout))
    });

    child.on('close', function (code) {
        clearTimeout(timeout);
        complete(code);
    });


    child.stdin.end();
}

// Get the time left in a set timeout
// http://stackoverflow.com/questions/3144711/find-the-time-left-in-a-settimeout
function getTimeLeft(timeout) {
    return Math.ceil((timeout._idleStart + timeout._idleTimeout - Date.now()) / 1000);
}

function reportBuffer(opts, buffer, strategies)
{
    if (buffer.stdout.join) buffer.stdout = buffer.stdout.join('');
    if (buffer.stderr.join) buffer.stderr = buffer.stderr.join('');

    if (strategies)
    {
        // added as a way to transform the buffer before any additional stdout/err specific processing is made.
        // useful for things like when you may want to split stdout into both stdout and stderr
        if (strategies.transformBuffer) {
            strategies.transformBuffer(buffer);
        }

        if (strategies.transformOutput)
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

    // return the output of the shell call
    if (opts.shellResult) {
        buffer.shellResult = opts.shellResult;
    }

    // escape the stderr output after strategies have been run
    // and before the it is written to the process stream
    
    buffer.outputType = strategies.outputType || 'pre';
    

    if (opts.format == 'json')
    {
        var json = JSON.stringify(buffer);
        writeToStream(process.stdout, json, "\\n");
    }
    else
    {
        if (buffer.stdout) writeToStream(process.stdout, buffer.stdout, "\n")
        if (buffer.stderr) writeToStream(process.stderr, buffer.stderr, "\n")
        if (buffer.wallTime && opts.debug) {
            console.info(buffer.wallTime + 'ms');
        }
    }
}

// we need to chunk the data back out to handle strange inconsistency issues with large data.
// Ideally we could chunk based off of character count but for some reason chunking by line breaks
// is the only thing that is consistent.
function writeToStream(stream, data, linebreak) {
    data.split(linebreak).forEach((line, i, arr) => {
        // don't write a line break on the last line
        return stream.write(line.normalize() + (i != arr.length - 1 ? linebreak : ''))
    });
}