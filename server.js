// TODO: Security still needs to be added to most endpoints

var express = require('express'),
    config = require('./lib/config'),
    docker = require('./lib/docker'),
    exec = require('child_process').exec,
    os = require('os');

var app = express();

//app.use(require('response-time')(5));
//app.use(require('connect-timeout')(10000));
app.use(require('body-parser')());

app.use(function(err, req, res, next) {
    console.log( err );
    err.request = req;
    res.send(500, { error: err })
});


app.get('/', function(req, res) {
    res.type( "text/plain" );
    res.send( 200, "Codewars Runner is live!" );
});

app.get('/version', function(req, res) {
    res.end(config.version);
});

app.get('/build', function(req, res) {
    exec('node build', function(err, stdout, stderr) {
        res.end(JSON.stringify({
           stdout: stdout,
           stderr: stderr
        }));
    });
});

// updates to the latest code and docker images
// responds to both get and post for backwards comparability and easier debugging.
getOrPost('/update', function(req, res)
{
    var updateSh = require('child_process').spawn('sh', [ 'setup/update.sh' ], {
        cwd: process.env.PWD
    });

    var buffer = [];

    updateSh.stdout.on('data', function(data)
    {
        buffer.push(data);
    });

    updateSh.stderr.on('data', function(data)
    {
        buffer.push(data);
    });

    updateSh.on('exit', function (code)
    {
        buffer.push(code);
        res.end(buffer.join(''));
    });

    useTimeout(1200000, res, buffer);
});

app.post('/run', function(req, res) {
    var image = req.body.image,
        taggedImage = docker.taggedImage(image);

    delete req.body.image;

    console.time(taggedImage);
    console.log(taggedImage);

    docker.run(image, 'run', req.body, function(error, data, stdout, stderr){
        console.timeEnd(taggedImage);

        json = safeParse(stdout);

        if (json) {
            json.freeMem = os.freemem();
            json.v = config.version;
            res.end(JSON.stringify(json));

        } else {
            console.warn('stdout did not return valid data, going with stderr');
            console.info(stderr);
            error = error || {};
            res.end(JSON.stringify({
                statusCode: error.statusCode,
                failed: {
                    stdout: stdout,
                    stderr: stderr,
                    reason: error.reason
                },
                freeMem: os.freemem(),
                v: config.version,
                details: error.json
            }));
        }
    });

    useTimeout(10000, res);
});

app.get('/status', function(req, res) {
    docker.run(null, 'run', {l: 'javascript', c: 'console.log(1+1)'}, function(err, data, stdout, stderr) {
        res.end(JSON.stringify({
            healthy: stdout.indexOf('stdout":"2\\n",') > 0,
            freeMem: os.freemem(),
            version: config.version,
            dryRun: {
                error: err && err.toString(),
                data: data,
                stdout: stdout,
                stderr: stderr
            }
        }));
    });

    useTimeout(5000, res);
});

function useTimeout(timeout, res, buffer) {
    setTimeout(function() {
        res.end(JSON.stringify({
            buffer: buffer,
            timeout: true,
            reason: 'Response timed out, took longer than ' + timeout + 'ms'
        }));
    }, timeout);
}

function safeParse(json) {
    try
    {
        return json ? JSON.parse(json) : null;
    }
    catch(ex)
    {
        console.log(ex);
        return null
    }
}

function getOrPost(route, cb) {
    app.get(route, cb);
    app.post(route, cb);
}

// for testing only
//app.post('/run', function(req, res) {
//    var image = req.body.image || 'codewars/cli-runner';
//    delete req.body.image;
//
//    docker.runCmd(image, 'run', req.body, function(error, out){
//        console.log(out);
//        res.end(out);
//    });
//});

server = app.listen(8080, function() {
    console.log('Listening on port %d', server.address().port);
});
