var docker = require('./lib/docker'),
    opts = require("nomnom")
        .options({
            image: {
                abbr: 'i',
                help: 'The image to build'
            },
            file: {
                abbr: 'f',
                help: 'The file to use to build the file'
            },
            push: {
                abbr: 'p',
                flag: true,
                help: 'Provide if image should be pushed after being built'
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

console.log('Building image, this may take a while...');
docker.build(opts.file, opts.image, function(err, stdout, stderr) {
    console.log(stdout);
    console.log(stderr);
    console.log('Image name = ' + docker.taggedImage(opts.image));

    if(opts.push) {
        console.log('Pushing image...');

        docker.push(opts.image, function(err, stdout, stderr) {
            console.log(stdout);
            console.log(stderr);
            console.log('Image pushed');
        });
    }
});

