require('fs').readFile('lib/runners/python.js', 'utf8', function (error, data) {
    if (error) throw error;
    eval(data.split('python').join('python3'));
});