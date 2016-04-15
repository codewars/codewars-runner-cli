var path = require('path'),
    pythonRunnerCode = require('fs').readFileSync(path.join(__dirname,'python.js'),'utf8');

eval(pythonRunnerCode.split('python').join('python3'));
