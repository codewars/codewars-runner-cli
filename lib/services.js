var spawn = require('child_process').spawn,
    Promise = require('bluebird'),
    temp = require('temp');

const startService = {
    redis: function() {
        return spawnAndWait('redis-server', [], 'Running')
    },
    mongodb: function() {
        var dir = temp.mkdirSync('mongodb')
        return spawnAndWait('mongod', ['--smallfiles', '--noprealloc', '--nojournal', '--dbpath',  dir], 'waiting for connections');
    }
}

function spawnAndWait(cmd, args, text) {
    return new Promise(function(resolve, reject) {
        var s = spawn(cmd, args);
        s.stdout.on('data', function(data) {
            var str = data && data.toString();
            // console.log(str);  // for debugging only

            if (str && str.indexOf(text)){
                resolve();
            }
        });

        s.on('exit', reject);

        setTimeout(reject, 5000);
    });
}

// loops through each requested service and returns once all services have started
module.exports.start = function(services, resolve) {
    return Promise.map(services || [], function(service){
        // console.log("Starting service " + service);
        return startService[service]();
    }).finally(resolve);
}