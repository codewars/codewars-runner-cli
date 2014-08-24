var docker = require('./lib/docker'),
    util = require('./lib/util'),
    config = require('./lib/config'),
    opts = require("nomnom")
        .options({
            push: {
                abbr: 'p',
                flag: true,
                help: 'Provide if image should be pushed after being built'
            },
            base: {
                abbr: 'b',
                flag: true,
                help: 'True if the base image should be built first'
            },
            version: {
                abbr: 'v',
                flag: true,
                help: 'Print version and exit',
                callback: function () {
                    return config.version
                }
            }
        })
        .help('This utility will rebuild the docker image for the latest version')
        .parse();


docker.imageChain(opts._, function(name, image, next, end)
{
    console.log("Name = " + name);
    console.log("Building " + image);

    docker.build(name, image, function (code)
    {
        console.log('Image name = ' + image);

        if (opts.push)
        {
            console.log('Pushing image ' + image + ' ...');

            docker.push(image, function (code)
            {
                console.log('Pushed ' + image);
                next();
            });
        }
        else
        {
            next();
        }
    });
}).run(function()
{
    // cleanup any resources
//    util.pipeSpawn('sh', ['setup/cleanup.sh']);
});


