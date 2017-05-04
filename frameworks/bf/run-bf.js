var execSync = require('child_process').execSync;
module.exports = function runBF(input) {
  if (input) {
    return execSync('bf /workspace/solution.txt', {input}).toString('utf8');
  }
  return execSync('bf /workspace/solution.txt').toString('utf8');
};
