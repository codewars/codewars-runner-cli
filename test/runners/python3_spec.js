describe('python3', function () {
    it('can evaluate exactly the same tests as the python2 runner', function (done) {
        require('fs').readFile('test/runners/python_spec.js', 'utf8', function (error, data) {
            if (error) throw error;
            eval(data.split('python').join('python3'));
            done();
        });
    });
});