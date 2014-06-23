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

// updates to the latest code and docker images
app.post('/update', function(req, res) {
    exec('git pull', function(err, stdout, stderr) {
        var git = stdout || stderr;

        docker.pull(null, function(err, stdout, stderr) {
            res.end(JSON.stringify({
                git: git,
                docker: stdout || stderr,
                version: config.version,
                image: docker.taggedImage()
            }));
        });
    });
});

app.post('/run', function(req, res) {
    var image = req.body.image;
    delete req.body.image;

    console.time(image);
    console.log(image);

    docker.run(image, 'run', req.body, function(error, data, stdout, stderr){
        console.timeEnd(image);

        if (stdout) {
            res.end(stdout);
        } else {
            console.warn('stdout did not return any data, going with stderr');
            console.info(stderr);
            error = error || {};
            res.end(JSON.stringify({
                stdout: '',
                stderr: stderr || error.reason,
                statusCode: error.statusCode,
                failed: true,
                details: error.json
            }));
        }
    });
});

app.get('/status', function(req, res) {
    docker.run(null, 'run', {l: 'javascript', c: 'console.log(1+1)'}, function(err, data, stdout, stderr) {
        res.end(JSON.stringify({
            healthy: stdout.indexOf('stdout":"2\\n",') > 0,
            freeMem: os.freemem(),
            dryRun: {
                error: err && err.toString(),
                data: data,
                stdout: stdout,
                stderr: stderr
            }
        }));
    });
});

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
