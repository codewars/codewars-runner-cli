var execSync = require('child_process').execSync;
module.exports = function runBF(input) {
  if (input) {
    return String.fromCharCode(...execSync('bf -c29999 /workspace/solution.txt', {input}));
  }
  return String.fromCharCode(...execSync('bf -c29999 /workspace/solution.txt'));
};
