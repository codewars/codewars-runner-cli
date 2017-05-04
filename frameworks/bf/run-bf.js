var execSync = require('child_process').execSync;
module.exports = function runBF(input) {
  if (typeof input != "undefined") {
    return execSync('bf /workspace/solution.txt', {input}).toString('utf8');
  }
  return execSync('bf /workspace/solution.txt').toString('utf8');
};
