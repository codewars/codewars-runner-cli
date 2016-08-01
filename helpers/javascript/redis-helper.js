var redis = require("redis"),
    Promise = require('bluebird');
    childProcess = require('child_process');

Promise.promisifyAll(redis.RedisClient.prototype);

module.exports.spawn = function spawnRedis() {
    return new Promise(function(resolve, reject) {
        var rs = childProcess.spawn('redis-server');
        rs.stdout.on('data', function(data) {
            if (data && data.toString().indexOf("Running")){
                resolve(function() {
                    process.exit();
                });
            }
        });

        rs.on('exit', reject);
    });
}