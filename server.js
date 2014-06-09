var express = require('express'),
    exec = require('child_process' ).exec,
    parseBodyToArgs = require('./lib/util').parseBodyToArgs,
    config = require('./lib/config');

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

app.post('/run', function(req, res) {
    var args = parseBodyToArgs(req.body ),
        cmd = config.cmds.dockerRun + ' ' + args;

    console.time(config.cmds.dockerRun);
    console.log(cmd);
    exec(cmd, {timeout: 10000}, function(error, stdout, stderr){
        console.timeEnd(config.cmds.dockerRun);
        res.end(stdout || stderr);
    });
});

server = app.listen(8080, function() {
    console.log('Listening on port %d', server.address().port);
});