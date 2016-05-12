var util = require('util'),
    spawn = require('child_process').spawn,
    config = require('./config'),
    os = require('os'),
    utf8 = require('utf8'),
    codeWriteSync = require('./util').codeWriteSync,
    temp = require('temp');

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
        // write the solution to a text file so that it can be inspected if need be
        codeWriteSync(null, opts.code, '/home/codewarrior/', 'solution.txt', true);

        runShell(opts, function() {
            (!opts.fixture) ? strategies.solutionOnly(runStrategy) : strategies.testIntegration(runStrategy);
        });
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

// handles running an optional shell value, which allows users to configure a shell script to be ran
// before the code executes
function runShell(opts, callback) {
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
            callback();
        });
    }
    else {
        callback();
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

    if (processStdin) child.stdin.write(processStdin);

    // Listen
    child.stdout.on('data', function (data)
    {
        if (!!data)
        {
            //console.log(data.toString());
            buffer.stdout.push(data.toString());
            stdoutLength += data.length;
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

    // return the output of the shell call
    if (opts.shellResult) {
        buffer.shell = opts.shellResult;
    }

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
        return stream.write(utf8.encode(line) + (i != arr.length - 1 ? linebreak : ''))
    });
}

//function writeToStream(stream, data, offset) {
//    var len = 1000, size = Math.ceil(data.length/len), offset = offset || 0;
//
//    for (var i = offset; i < size; i++) {
//        offset = i * len;
//        chunk = data.substring(offset, offset + len);
//        stream.write(chunk)
//        // stream.write always returns true yet it doesnt always output everything correctly.
//        if (false) {
//            stream.once('drain', function() {
//                //writeToStream(stream, data, offset);
//            });
//            return;
//        }
//    }
//}
