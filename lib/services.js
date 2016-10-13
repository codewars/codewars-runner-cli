var spawn = require('child_process').spawn,
    Promise = require('bluebird'),
    temp = require('temp');

const startService = {
    redis: function() {
        return spawnAndWait('redis-server', [], 'Running')
    },
    mongodb: function() {
        var dir = temp.mkdirSync('mongodb')
        return spawnAndWait('mongod', ['--smallfiles', '--noprealloc', '--nojournal', '--dbpath',  dir], 'waiting for connections on port');
    },
    postgres: function() {
        return spawnAndWait('postgres', ['-D', '/home/codewarrior/pg', '-n', '-F'], 'ready to accept connections')
    },
    mariadb: function() {
        // TODO
    },
    couchdb: function() {
        // TODO
    },
    rabbitmq: function() {
        // TODO
    }
}

function spawnAndWait(cmd, args, text, timeout) {

    return new Promise(function(resolve, reject) {

        function handleData(data) {
            var str = data && data.toString();
            // console.log(str);  // for debugging only

            if (str && str.indexOf(text) > 0){
                resolve();
            }
        }

        var s = spawn(cmd, args);
        s.stderr.on('data', handleData);
        s.stdout.on('data', handleData);

        s.on('exit', resolve);

        setTimeout(resolve, timeout || 2000);
    });
}

// loops through each requested service and returns once all services have started
module.exports.start = function(services, resolve) {
    return Promise.map(services || [], function(service){
        // console.log("Starting " + service); // debug only
        return startService[service]();
    }).finally(resolve);
}