var spawn = require('child_process').spawn,
  Promise = require('bluebird'),
  temp = require('temp');

const startService = {
  redis: function(opts) {
    return spawnAndWait(opts, 'redis-server', [], 'Running');
  },
  mongodb: function(opts) {
    var dir = temp.mkdirSync('mongodb');
    return spawnAndWait(opts, 'mongod', ['--smallfiles', '--noprealloc', '--nojournal', '--dbpath', dir], 'waiting for connections on port');
  },
  postgres: function(opts) {
    // stop the service. This is mostly needed just for specs since under normal circumstances the entire
    // container would be destroyed and no cleanup would be necessary
    opts.onCompleted.push(function() {
      spawn("/usr/lib/postgresql/9.6/bin/pg_ctl", ['-D', '/pgdata/pg', '-w', 'stop']);
    });

    return spawnAndWait(opts, '/usr/lib/postgresql/9.6/bin/pg_ctl', ['-D', '/pgdata/pg', '-w', 'restart']);
  },
  mariadb: function(opts) { // eslint-disable-line no-unused-vars
    // TODO
  },
  couchdb: function(opts) { // eslint-disable-line no-unused-vars
    // TODO
  },
  rabbitmq: function(opts) { // eslint-disable-line no-unused-vars
    // TODO
  }
};

function spawnAndWait(opts, cmd, args, text, delay) {
  return new Promise(function(resolve, reject) { // eslint-disable-line no-unused-vars
    function handleData(data) {
      var str = data && data.toString();
      opts.publish('stdout', str);
      // console.log(str);  // for debugging only

      if (str && str.indexOf(text) > 0) {
        setTimeout(resolve, delay || 0);
      }
    }

    var s = spawn(cmd, args);
    s.stderr.on('data', handleData);
    s.stdout.on('data', handleData);
    s.on('exit', resolve);
    setTimeout(resolve, 2000);
  });
}

// loops through each requested service and returns once all services have started
module.exports.start = function(opts, resolve) {
  return Promise.map(opts.services || [], function(service) {
    // console.log("Starting " + service); // debug only
    return startService[service](opts);
  }).finally(resolve);
};
