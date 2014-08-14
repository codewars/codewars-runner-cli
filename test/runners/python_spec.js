var expect = require('chai').expect;
var runner = require('../../lib/runners/python');

describe('python runner2', function () {
    this.timeout(600);

    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            runner.run({language: 'python', solution: 'print 42'}, function (buffer) {
                expect(buffer.stdout).to.equal('42\n');
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
            runner.run({language: 'python',
                    solution: 'a = 1',
                    fixture: 'test.assert_equals(a, 1)',
                    testFramework: 'cw-2'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
        });
        it('should handle a basic setup', function (done) {
            runner.run({language: 'python',
                    solution: 'a = 1',
                    setup: 'b = 2',
                    fixture: 'test.assert_equals(b, 2)',
                    testFramework: 'cw-2'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
        });
        it('should handle a failed assertion', function (done) {
            runner.run({language: 'python',
                    solution: 'a = 1',
                    fixture: 'test.expect(a == 2)',
                    testFramework: 'cw-2'},
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
                    fixture: 'class TestSequenceFunctions(unittest.TestCase):\n'
                        + '  def test_assert(self):\n'
                        + '    self.assertEqual(a, 1)\n'
                        + '_testsuite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)',
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
                    fixture: 'class TestSequenceFunctions(unittest.TestCase):\n'
                        + '  def test_assert(self):\n'
                        + '    self.assertEqual(a, 2, "test failed")\n'
                        + '_testsuite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)',
                    testFramework: 'unittest'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<FAILED::>test failed\n');
                    done();
                });
        });
        it('should handle a failed assetion', function (done) {
            runner.run({language: 'python',
                    solution: 'a = 1',
                    fixture: 'class TestSequenceFunctions(unittest.TestCase):\n'
                        + '  def test_assert(self):\n'
                        + '    raise StandardError("exception")\n'
                        + '_testsuite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)',
                    testFramework: 'unittest'},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<ERROR::>Unhandled Exception: exception\n');
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
                        'mongod = subprocess.Popen("mongod", stdout=subprocess.PIPE, stderr=subprocess.STDOUT)',
                        'while True:',
                        '  l = mongod.stdout.readline()',
                        '  m = re.match(r".*waiting for connections on port 27017", l)',
                        '  if m:',
                        '    print l',
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
