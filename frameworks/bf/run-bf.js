var execSync = require('child_process').execSync;
module.exports = function runBF(input) {
  if (input) {
    return execSync('bf /workspace/solution.txt', {input, encoding: 'utf8'});
  }
  return execSync('bf /workspace/solution.txt', {encoding: 'utf8'});
};
