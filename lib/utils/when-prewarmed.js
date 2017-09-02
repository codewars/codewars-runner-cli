const fs = require('fs-extra');
const execSync = require('child_process').execSync;

const statusPath = "/home/codewarrior/prewarm.status";

/**
 * If the prewarming process is in-process, we want to wait until it finishes, otherwise in the case of things like waiting
 * for Gradle, two builds happen and that will kill performance and the process will likely fail due to a timeout.

 * @param cb Function callback. True will be passed to the callback if the prewarm happened, which
 *                    will indicate that the deamon should have been started.
 */
function whenPrewarmed(opts, cb, notified) {
  if (!fs.pathExistsSync(statusPath)) {
    cb(false);
  }
  else if (fs.readFileSync(statusPath).toString().indexOf('loaded') === 0) {
    cb(true);
  }
  else {
    if (!notified && opts.publish) {
      opts.publish('status', 'Waiting for prewarming to finish...');
    }
    setTimeout(() => whenPrewarmed(opts, cb, true), 200);
  }
}

// used by specs to ensure the daemon is prewarmed
function ensure(done, shPath = "/runner/prewarm.sh") {
  if (!fs.pathExistsSync(statusPath)) {
    console.log("Starting daemon with test run to ensure tests run within their allowed time...");
    console.log(execSync(`sh ${shPath}`).toString());
    done();
  }
  else {
    whenPrewarmed({}, done);
  }
}

function clean() {
  execSync(`rm -rf ${statusPath}`);
  execSync(`gradle --stop`);
}

module.exports = whenPrewarmed;
module.exports.ensure = ensure;
module.exports.clean = clean;

// sets up mocha, also ensures that done never gets called with an argument
module.exports.setupMocha = function() {
  before(done => ensure(_ => done()));
  after(clean);
};
