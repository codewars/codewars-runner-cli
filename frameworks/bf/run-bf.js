var execSync = require('child_process').execSync;
module.exports.runBf = function(input) {
  if (typeof input != "undefined") {
    return execSync('bf /workspace/solution.txt', {input});
  }
  return execSync('bf /workspace/solution.txt');
};
