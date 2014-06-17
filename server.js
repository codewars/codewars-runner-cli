var express = require('express'),
    run = require('./lib/docker' ).run;

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
    var image = req.body.image || 'codewars/cli-runner';
    delete req.body.image;

    console.time(image);
    console.log(image);

    run(image, 'run', req.body, function(error, out){
        console.timeEnd(image);
        res.end(out);
    });
});

server = app.listen(8080, function() {
    console.log('Listening on port %d', server.address().port);
});
