var exec = require('child_process').exec,
    util = require('./util'),
    config = require('./config'),
    Chain = new require('./chain'),
    Docker = require('dockerode'),
    Streams = require('memory-streams'),
    docker = new Docker({socketPath: '/var/run/docker.sock'}),
    fs = require('fs');


module.exports.run = function run(image, cmd, args, cb)
{
    args.format = args.format || 'json';
    args = util.parseBodyToArray(args);
    args.unshift(cmd);

    var stderr = new Streams.WritableStream(),
        stdout = new Streams.WritableStream();
    docker.run(taggedImage(image), args, [stdout, stderr], {Tty: false}, function (error, data, container)
    {
        // make sure the container is removed
        if (container)
        {
            container.remove(function (err, data)
            {
                console.log("container removed " + (err || data));
            });
        }

        cb(error, data, stdout.toString(), stderr.toString());
    });
};

// runs using the cli instead of remote api.
module.exports.runCmd = function runCmd(image, cmd, args, cb)
{
    args = util.parseBodyToArgs(args);
    var cmd = 'docker run --rm  ' + taggedImage(image) + ' ' + cmd + ' --format json ' + args;
    exec(cmd, {timeout: 10000}, function (error, stdout, stderr)
    {
        cb(error, stdout || stderr);
    });
};

// simple utility for building images with their tag automatically set to the configured version
module.exports.build = function build(name, image, cb)
{
    var target = "./docker/" + name + ".docker",
        source = "./Dockerfile";
    console.log('Copying ' + target + ' to root for build');
    fs.writeFileSync(source, fs.readFileSync(target));

    util.pipeSpawn('docker', ['build', '-t', image, '.'], function()
    {
        fs.unlinkSync(source);
        if(cb) cb();
    });
};

// simple utility for pushing images with their tag automatically set to the configured version
module.exports.push = function build(image, cb)
{
    util.pipeSpawn('docker', ['push', image], cb);
};

module.exports.pull = function pull(image, cb)
{
    util.pipeSpawn('docker', ['pull', image], cb);
};

function imageName(nameOrLanguage)
{
    if (nameOrLanguage == 'base')
    {
        return 'base';
    }
    else
    {
        var items = Object.keys(config.images).filter(function (key)
        {
            return key == nameOrLanguage || config.images[key].indexOf(nameOrLanguage) >= 0;
        });

        return items ? items[0] : null;
    }
}

module.exports.imageName = imageName;

// returns the image tagged with the current version.
function taggedImage(nameOrLanguage)
{
    if (nameOrLanguage.indexOf('codewars/') == 0)
    {
        return nameOrLanguage;
    }
    else
    {
        var image = 'codewars/runner-' + imageName(nameOrLanguage);
        return image + (nameOrLanguage == 'base' ? '' : ':' + config.version);
    }
}
module.exports.taggedImage = taggedImage;

function imageNames(names)
{
    if (names)
    {
        if (Array.isArray(names))
        {
            if (names.length == 0)
            {
                return Object.keys(config.images);
            }
            else
            {
                return names.map(function(name)
                {
                    return imageName(name);
                });
            }
        }
        else
        {
            return [imageName(names)];
        }
    }
    else
    {
        return Object.keys(config.images);
    }
}
module.exports.imageNames = imageNames;

// Convenience method that builds a callback chain for the image names given.
module.exports.imageChain = function imageChain(names, handler)
{
    return new Chain(imageNames(names).map(function(name)
    {
        if (!name) return function (next)
        {
            next()
        }
        return function (next, done)
        {
            handler(name, taggedImage(name), next, done);
        }
    }));
}