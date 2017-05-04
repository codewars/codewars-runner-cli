var execSync = require('child_process').execSync;
module.exports = function runBF(input) {
  if (typeof input != "undefined") {
    return execSync('bf /workspace/solution.txt', {input})/* .replace(/^\<Buffer |\>$/g, "") */;
  }
  return execSync('bf /workspace/solution.txt')/* .replace(/^\<Buffer |\>$/g, "") */;
};
