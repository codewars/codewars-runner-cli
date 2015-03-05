var expect = require('chai').expect;
var runner = require('../../lib/runners/python');

describe('python runner', function () {

    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            runner.run({language: 'python', solution: 'import sys; sys.stdout.write("42")'}, function (buffer) {
                console.log(buffer);
                expect(buffer.stdout).to.equal('42');
                done();
            });
        });
    });
    describe('cw-2', function () {
        it('should handle a basic assertion', function (done) {
            runner.run({
                    language: 'python',
                    solution: 'a = 1',
                    fixture: 'test.expect(a == 1)',
                    testFramework: 'cw-2'
                },
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
        });
        it('should handle a basic assert_equals', function (done) {
            runner.run({
                    language: 'python',
                    solution: 'a = 1',
                    fixture: 'test.assert_equals(a, 1)',
                    testFramework: 'cw-2'
                },
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
        });
        it('should handle a basic setup', function (done) {
            runner.run({
                    language: 'python',
                    solution: 'a = 1',
                    setup: 'b = 2',
                    fixture: 'test.assert_equals(b, 2)',
                    testFramework: 'cw-2'
                },
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
        });
        it('should handle a failed assertion', function (done) {
            runner.run({
                    language: 'python',
                    solution: 'a = 1',
                    fixture: 'test.expect(a == 2)',
                    testFramework: 'cw-2'
                },
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<FAILED::>Value is not what was expected\n');
                    done();
                });
        });

        it('should handle a failed assertion', function (done) {
            runner.run({language: 'python',
                    solution: 'a.fail()',
                    testFramework: 'cw-2'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stderr).to.not.contain('File ');
                    expect(buffer.stderr).to.not.contain(', line ');
                    expect(buffer.stderr).to.not.contain('most recent call last');
                    done();
                });
        });
    });
    describe('unittest', function () {
        it('should handle a basic assertion', function (done) {
            runner.run({language: 'python',
                    solution: 'a = 1',
                    fixture: [
                        'class Test(unittest.TestCase):',
                        '  def test_assert(self):',
                        '    self.assertEqual(a, 1)'
                    ].join('\n'),
                    testFramework: 'unittest'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
        });
        it('should handle a failed assetion', function (done) {
            runner.run({language: 'python',
                    solution: 'a = 1',
                    fixture: [
                        'class Test(unittest.TestCase):',
                        '  def test_assert(self):',
                        '    self.assertEqual(a, 2, "test failed")'
                    ].join('\n'),
                    testFramework: 'unittest'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.contain('<FAILED::>');
                    expect(buffer.stdout).to.contain('test failed');
                    done();
                });
        });
        it('should handle a failed assetion', function (done) {
            runner.run({language: 'python',
                    solution: 'a = 1',
                    fixture: [
                        'class Test(unittest.TestCase):',
                        '  def test_assert(self):',
                        '    raise Exception("exceptions are my favorite, I always throw them")'
                    ].join('\n'),
                    testFramework: 'unittest'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<ERROR::>Unhandled Exception: exceptions are my favorite, I always throw them\n');
                    done();
                });
        });
    });
    describe('potpourri', function () {
        it('can run mongodb', function (done) {
            this.timeout(5000);
            runner.run({
                    language: 'python',
                    setup: [
                        'import subprocess',
                        'import re',
                        'import sys',
                        'mongod = subprocess.Popen("mongod", stdout=subprocess.PIPE, stderr=subprocess.STDOUT)',
                        'is_waiting = re.compile(r".*waiting for connections on port 27017")',
                        'while True:',
                        '  l = mongod.stdout.readline().decode("utf-8")',
                        '  if is_waiting.match(l):',
                        '    sys.stdout.write(l + "\\n")',
                        '    break'
                    ].join('\n'),
                    solution: [
                        'from pymongo import MongoClient',
                        'with MongoClient() as client:',
                        '  table = client["HelloMongoCollection"]["HelloMongoTable"]',
                        '  table.drop()',
                        "  table.insert({'_id': 42, 'name': 'My Document', 'ids': [1,2,3], 'subdocument': {'a':2}})"
                    ].join('\n'),
                    fixture: [
                        'from pymongo import MongoClient',
                        'with MongoClient() as client:',
                        '  table = client["HelloMongoCollection"]["HelloMongoTable"]',
                        "  test.assert_equals(list(table.find()), [{u'_id': 42, u'name': u'My Document', u'subdocument': {u'a': 2}, u'ids': [1, 2, 3]}])",
                        'mongod.terminate()'
                    ].join('\n'),
                    testFramework: 'cw-2'
                },
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.contain('waiting for connections on port 27017');
                    expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                    done();
                });
        });
    });
});
