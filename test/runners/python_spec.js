var expect = require('chai').expect;
var runner = require('../../lib/runners/python');

describe('python runner', function () {

    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            runner.run({language: 'python', solution: 'from __future__ import print_function ; print(42)'}, function (buffer) {
                console.log(buffer);
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
        it('version should be either 2.7.6 or 3.4.1', function (done) {
            runner.run({
                language: 'python',
                solution: [
                    "from sys import version_info",
                    "if version_info in [(2,7,6,'final',0), (3,4,1,'final',0)]:",
                    "  print ('Version is apparently either 2.7.6 or 3.4.1')",
                    "else:",
                    "  print ('BAD VERSION: {0}'.format(version_info))"
                ].join('\n')
            }, function (buffer) {
                console.log(buffer);
                expect(buffer.stdout).to.equal('Version is apparently either 2.7.6 or 3.4.1\n');
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
                }, function (buffer) {
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
                }, function (buffer) {
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
                }, function (buffer) {
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
                }, function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('<FAILED::>Value is not what was expected\n');
                    done();
                });
        });

        it('should handle a failed assertion', function (done) {
            runner.run({language: 'python',
                    solution: 'a.fail()',
                    testFramework: 'cw-2'
                }, function (buffer) {
                    console.log(buffer);
                    expect(buffer.stderr).to.not.contain('File ');
                    expect(buffer.stderr).to.not.contain(', line ');
                    expect(buffer.stderr).to.not.contain('most recent call last');
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
                        'from sys import stdout',
                        'import subprocess',
                        'import re',
                        'mongod = subprocess.Popen("mongod", stdout=subprocess.PIPE, stderr=subprocess.STDOUT)',
                        'is_waiting = re.compile(r".*waiting for connections on port 27017")',
                        'while True:',
                        '  l = mongod.stdout.readline().decode("utf-8")',
                        '  m = is_waiting.match(l)',
                        '  if m:',
                        '    stdout.write(l)',
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
