var util = require('./lib/util'),
    docker = require('./lib/docker'),
    config = require('./lib/config'),
    opts = require("nomnom")
        .options({
            version: {
                abbr: 'v',
                flag: true,
                help: 'Print version and exit',
                callback: function () {
                    return config.version
                }
            }
        })
        .help('This utility will test the specified docker image')
        .parse();

// i.e. docker run -i --entrypoint mocha codewars/cli-runner:0.1.22 --recursive -t 5000
docker.imageChain(opts._, function(name, image, next)
{
    console.log("Testing " + image);

    var args = ["run", "-i", "--entrypoint", "mocha", image, "-t", 5000],
        specs;

    // support the ability to test a specific language.
    if (opts._.length == 1 && opts[0] != name)
    {
        specs = ["test/runners/" + opts._[0] + "_spec.js"];
    }
    else
    {
        specs = config.images[name].map(function (language)
        {
            return "test/runners/" + language + "_spec.js";
        });
    }

    util.pipeSpawn("docker", args.concat(specs), function()
    {
        // Use a slight pause so that the results can be seen during multi-language tests
        setTimeout(next, 1000);
    });

}).run();

