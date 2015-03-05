//! Simple utility for pushing latest docker image with version tagged

var docker = require('./lib/docker'),
    opts = require("nomnom")
        .options({
            image: {
                abbr: 'i',
                help: 'The image to push. No tag needed, it will automatically be appended. If empty the default will be used.'
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

// if no items where provided then push everything, including base
//if (opts._.length == 0)
//{
//    opts._ = docker.imageNames();
//    opts._.unshift('base');
//}

docker.imageChain(opts._, function(name, image, next)
{
    console.log("Pushing " + image);
    docker.push(image, function(code) {
        console.log("Push " + image + " finished with code " + code);
        next();
    });
}).run();

