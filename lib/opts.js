var fs = require('fs'),
    config = require('./config'),
    then = require('./util').then;

function processLanguage(opts, cb)
{

    switch (opts.language)
    {
        case 'lua':
            opts.language = 'lua';
            opts.ext = 'lua';
            break;

        case 'ml':
        case 'ocaml':
            opts.language = 'ocaml';
            opts.ext = 'ml';
            break;

        case 'ex':
        case 'elixir':
            opts.language = 'elixir';
            opts.ext = 'ex';
            break;

        case 'objc':
        case 'obj-c':
        case 'objective-c':
            opts.language = 'objc';
            opts.ext = 'm';
            break;

        case 'r':
            opts.language = 'r';
            opts.ext = 'r';
            break;

        case 'swift':
            opts.language = 'swift';
            opts.ext = 'swift';
            break;

        case 'asm':
        case 'nasm':
            opts.language = 'nasm';
            opts.ext = 'asm';
            break;

        case 'gas':
            opts.language = 'gas';
            opts.ext = 's';
            break;

        case 'rs':
        case 'rust':
            opts.language = 'rust';
            opts.ext = 'rs';
            break;

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

        case 'ts':
        case 'typescript':
            opts.language = 'typescript';
            opts.ext = 'ts';
            break;

        case 'hs':
        case 'haskell':
            opts.language = 'haskell';
            opts.ext = 'hs';
            break;

        case 'clj':
        case 'clojure':
            opts.language = 'clojure';
            opts.ext = 'clj';
            break;

        case 'cl':
        case 'lisp':
            opts.language = 'lisp';
            opts.ext = 'lisp';
            break;

        case 'rkt':
        case 'racket':
            opts.language = 'racket';
            opts.ext = 'rkt';
            break;

        case 'c':
            opts.language = 'c';
            opts.ext = 'c';
            break;

        case 'c++':
        case 'cpp':
        case 'cplusplus':
            opts.language = 'c++';
            opts.ext = 'cpp';
            break;

	case 'groovy':
            opts.language = 'groovy';
            opts.ext = 'groovy';
            break;

        case 'jl':
        case 'julia':
            opts.language = 'julia';
            opts.ext = 'jl';
            break;

        case 'erl':
        case 'erlang':
            opts.language = 'erlang';
            opts.ext = 'erl';
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

        case 'python3':
            opts.language = 'python3';
            opts.ext = 'py';
            break;

        case 'php':
            opts.language = 'php';
            opts.ext = 'php';
            break;

        case 'go':
            opts.language = 'go';
            opts.ext = 'go';
            break;

        case 'cs':
        case 'csharp':
            opts.language = 'csharp';
            opts.ext = 'cs';
            break;

        case 'fsx':
        case 'fsharp':
            opts.language = 'fsharp';
            opts.ext = 'fsx';
            break;

        case 'bash':
        case 'sh':
            opts.language = 'bash';
            opts.ext = 'bs';
            break;

        case 'perl':
        case 'pl':
            opts.language = 'perl';
            opts.ext = 'pl';
            break;

        default:
            return cb('language is missing or not supported');
    }

    cb();
}

// checks one of the code options (setup, solution, fixture, etc),
// converts any inline code into a file, validates the argument if its required.
function processCodeOption(key, opts, required, cb)
{
    var file_key = key + 'File';

    if (required && !opts[key] && !opts[file_key])
    {
        cb(key + ' or ' + file_key + ' must be specified');
    }
    // if a file was provided then we will read it in, as we will likely need to modify it later
    else if (opts[file_key])
    {
        fs.readFile(opts[file_key], 'utf-8', function (err, data)
        {
            if (err)
            {
                cb(err);
            }
            else if (!data)
            {
                cb(key + ' file was not found or has no data');
            }
            else
            {
                opts[key] = data;
                cb();
            }
        });
    }
    else
    {
        cb();
    }
}

module.exports.process = function process(opts, cb)
{
    processLanguage(opts, function ()
    {
        processCodeOption('setup', opts, false, then(cb, function ()
        {
            processCodeOption('solution', opts, true, then(cb, function ()
            {
                processCodeOption('fixture', opts, false, then(cb, function ()
                {
                    // set the default test framework if it isn't already set
                    opts.testFramework = opts.testFramework || config.testFramework.defaults[opts.language];

                    if (opts.debug) console.info('Processed options: ', opts);
                    cb(opts);
                }));
            }));
        }));
    });
};


