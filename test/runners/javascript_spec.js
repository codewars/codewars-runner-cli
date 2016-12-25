var expect = require('chai').expect,
    runner = require('../runner');

describe( 'javascript runner', function(){
    describe( '.run', function(){
        runner.assertCodeExamples('javascript');
        
        //----------------------------------------------------------------------------------------
        // Basics
        //----------------------------------------------------------------------------------------

        describe( 'basics', function() {
            it('should handle basic code evaluation', function (done) {
                runner.run({language: 'javascript', code: 'console.log(42)'}, function (buffer) {
                    expect(buffer.stdout).to.equal('42\n');
                    done();
                });
            });
            it('should handle JSON.stringify ', function (done) {
                runner.run({language: 'javascript', code: 'console.log(JSON.stringify({a: 1}))'}, function (buffer) {
                    console.log(buffer.stderr);
                    expect(buffer.stdout).to.contain('{"a":1}');
                    done();
                });
            });

            it('should handle unicode characters', function (done) {
                runner.run({language: 'javascript', code: 'console.log("✓")'}, function (buffer) {
                    expect(buffer.stdout).to.include("✓");
                    done();
                });
            });

            it('should be able to access solution.txt', function (done) {
                runner.run({
                    language: 'javascript',
                    code: `
                        console.log(1+4);
                        console.log(require('fs').readFileSync('/home/codewarrior/solution.txt', 'utf8'));
                    `
                }, function (buffer) {
                    expect(buffer.stdout).to.contain("5");
                    expect(buffer.stdout).to.contain("1+4");
                    done();
                });
            });
            it('should allow a shell script to be ran', function (done) {
                runner.run({
                    language: 'javascript',
                    shell: 'echo "test 123" >> /home/codewarrior/test.txt ; ls',
                    code: `
                        console.log(require('fs').readFileSync('/home/codewarrior/test.txt', 'utf8'));
                    `
                }, function (buffer) {
                    expect(buffer.stdout).to.contain("test 123");
                    expect(buffer.shellResult.stdout.length).to.be.gt(0);
                    done();
                });
            });

            // it('should be able to handle large output data', function (done) {
            //     runner.run({
            //         language: 'javascript',
            //         code: `
            //             for(i = 0;i < 9999; i++){
            //                 console.log(i * 10);
            //             }
            //         `
            //     }, function (buffer) {
            //         expect(buffer.stderr).to.equal('');
            //         done();
            //     });
            // });

            it('should handle es6 code evaluation', function (done) {
                runner.run({language: 'javascript', code: 'let a = 42; console.log(42);'}, function (buffer) {
                    expect(buffer.stdout).to.equal('42\n');
                    done();
                });
            });

            it('should handle bad babel syntax', function (done) {
                runner.run({
                    language: 'javascript',
                    languageVersion: '6.x/babel',
                    code: 'var a = function(){returns 42;};\na();'
                }, function (buffer) {
                    expect(buffer.stderr).to.contain('Unexpected token');
                    done();
                });
            });

            it('should handle mongodb service with mongoose', function (done) {
                runner.run({
                    language: 'javascript',
                    services: ['mongodb'],
                    code: `
                        var mongoose = require('mongoose');
                        mongoose.Promise = global.Promise;
                        mongoose.connect('mongodb://localhost/spec');
                        var Cat = mongoose.model('Cat', { name: String });
        
                        var kitty = new Cat({ name: 'Zildjian' });
                        kitty.save(function (err) {
                          if (err) {
                            console.log(err);
                          } else {
                            console.log('meow');
                          }
                          process.exit();
                        });
                    `
                }, function (buffer) {
                    expect(buffer.stdout).to.contain('meow');
                    done();
                });
            });

            it('should handle redis service', function (done) {
                runner.run({
                    language: 'javascript',
                    services: ['redis'],
                    code: `
                        var redis = require('redis'),
                            Promise = require('bluebird');
        
                        Promise.promisifyAll(redis.RedisClient.prototype);
                        var client = redis.createClient();
        
                        client.setAsync("foo", "bar").then(_ => {
                            client.getAsync("foo").then( v => {
                                console.log(v);
                                process.exit();
                            })
                        });
                    `
                }, function (buffer) {
                    expect(buffer.stdout).to.contain('bar');
                    done();
                });
            });

            it('should handle react syntax', function (done) {
                runner.run({
                        language: 'javascript',
                        languageVersion: '6.6.0/babel',
                        code: `
                        var React = require("react");
                        var ReactDOM = require("react-dom/server");
                        let render = (el) => ReactDOM.renderToStaticMarkup(el);
                        var div = <div><h3>Test</h3></div>;
                        console.log(render(div));
                    `
                    },
                    function (buffer) {
                        expect(buffer.stdout).to.contain('<div><h3>Test</h3></div>');
                        done();
                    });
            });

            it('should handle react syntax using 0.10.x', function (done) {
                runner.run({
                        language: 'javascript',
                        languageVersion: '0.10.x/babel',
                        code: `
                        var React = require("react");
                        var ReactDOM = require("react-dom/server");
                        let render = (el) => ReactDOM.renderToStaticMarkup(el);
                        var div = <div><h3>Test</h3></div>;
                        console.log(render(div));
                    `
                    },
                    function (buffer) {
                        expect(buffer.stdout).to.contain('<div><h3>Test</h3></div>');
                        done();
                    });
            });

            it('should load libraries', function (done) {
                runner.run({
                    language: 'javascript',
                    code: 'var _ = require("lodash");console.log(_.map([1], n => n * 2));'
                }, function (buffer) {
                    expect(buffer.stdout).to.contain('[ 2 ]');
                    done();
                });
            });

            it('should work with SQLite', function (done) {
                runner.run({
                    language: 'javascript',
                    code: `
                        var sqlite3 = require('sqlite3');
                        var db = new sqlite3.Database(':memory:');
                        db.serialize(function() {
                          db.run("CREATE TABLE lorem (info TEXT)");
                          var stmt = db.prepare("INSERT INTO lorem VALUES (?)");
                          for (var i = 0; i < 10; i++) {
                              stmt.run("Ipsum " + i);
                          }
                          stmt.finalize();
                          db.each("SELECT rowid AS id, info FROM lorem", function(err, row) {
                              console.log(row.id + ": " + row.info);
                          });
                        });
        
                        db.close();
                    `
                }, function (buffer) {
                    expect(buffer.stdout).to.contain('Ipsum 0');
                    expect(buffer.stdout).to.contain('Ipsum 9');
                    done();
                });
            });

            it('should handle stderr', function (done) {
                runner.run({language: 'javascript', code: 'console.error("404 Not Found")'}, function (buffer) {
                    expect(buffer.stderr).to.equal('404 Not Found\n');
                    done();
                });
            });

            it('should handle stdout and stderr', function (done) {
                runner.run({
                    language: 'javascript',
                    code: 'console.log("stdout"); console.error("stderr")'
                }, function (buffer) {
                    expect(buffer.stdout).to.equal('stdout\n');
                    expect(buffer.stderr).to.equal('stderr\n');
                    done();
                });
            });
        });


        //----------------------------------------------------------------------------------------
        // Mocha BDD
        //----------------------------------------------------------------------------------------

        describe('mocha bdd', function() {
            it( 'should handle outputting objects', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var a = {b: 2};console.log(this);',
                    fixture: 'var assert = require("chai").assert;describe("test", function(){it("should be 2", function(){assert.equal(2, a.b);})});',
                    testFramework: 'mocha_bdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<PASSED::>');
                        done();
                    });
            });
            it( 'should handle no trailing semicolons', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var a = 2',
                    fixture: 'var assert = require("chai").assert;describe("test", function(){it("should be 2", function(){assert.equal(2, a);})});',
                    testFramework: 'mocha_bdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<PASSED::>');
                        done();
                    });
            });
            it( 'should handle failures', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var a = {b: 2};',
                    fixture: 'var assert = require("chai").assert;describe("test", function(){describe("failures", function(){it("should be 1", function(){assert.equal(1, a.b);})})});',
                    testFramework: 'mocha_bdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<FAILED::>');
                        done();
                    });
            });

            it( 'should handle errors', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var a = {b: 2};',
                    fixture: 'describe("test", function(){describe("failures", function(){it("should be 1", function(){throw new Error("test error");})})});',
                    testFramework: 'mocha_bdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<ERROR::>');
                        done();
                    });
            });

            it( 'should support options files', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var name = require("./config.js").name',
                    fixture: `
                        var expect = require('chai').expect;
                        describe("files", function(){
                            it("should be able to require the file", function(){
                                expect(name).to.equal("example");
                            });
                        });
                    `,
                    files: {
                        "config.js": "module.exports.name = 'example';"
                    },
                    testFramework: 'mocha_bdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<PASSED::>');
                        done();
                    });
            });

            it( 'should load chai-display', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'global.name = "example";',
                    fixture: `
                        var expect = require('chai').expect;
                        require("chai-display");
                        describe("files", function(){
                            it("should be able to require the library", function(){
                                expect(global.name).to.equal("example");
                            });
                        });
                    `,
                    testFramework: 'mocha_bdd'},
                    function(buffer) {
                        console.log(buffer);
                        expect(buffer.stdout).to.contain('<PASSED::>');
                        done();
                    });
            });
        });


        //----------------------------------------------------------------------------------------
        // Mocha TDD
        //----------------------------------------------------------------------------------------

        describe('mocha tdd', function() {
            it( 'should handle outputting objects', function(done){
                runner.run({
                        language: 'javascript',
                        code: 'var a = {b: 2};console.log(this);',
                        fixture: 'var assert = require("chai").assert;suite("test", function(){test("should be 2", function(){assert.equal(2, a.b);})});',
                        testFramework: 'mocha_tdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<PASSED::>');
                        done();
                    });
            });
            it( 'should handle failures', function(done){
                runner.run({
                        language: 'javascript',
                        code: 'var a = {b: 2};',
                        fixture: 'var assert = require("assert"); suite("test", function(){suite("failures", function(){test("should be 1", function(){assert.equal(1, a.b);})})});',
                        testFramework: 'mocha_tdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<FAILED::>');
                        done();
                    });
            });
            it( 'should handle chai failures', function(done){
                runner.run({
                        language: 'javascript',
                        code: 'var a = {b: 2};',
                        fixture: 'var assert = require("chai").assert; suite("test", function(){suite("failures", function(){test("should be 1", function(){assert.equal(1, a.b);})})});',
                        testFramework: 'mocha_tdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<FAILED::>');
                        done();
                    });
            });
            it( 'should handle errors', function(done){
                runner.run({
                        language: 'javascript',
                        code: 'var a = {b: 2};',
                        fixture: 'suite("test", function(){suite("failures", function(){test("should be 1", function(){throw new Error("test error");})})});',
                        testFramework: 'mocha_tdd'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<ERROR::>');
                        done();
                    });
            });
        });


        //----------------------------------------------------------------------------------------
        // CW-2
        //----------------------------------------------------------------------------------------

        describe('cw-2', function() {
            it( 'should handle outputting objects', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var a = {b: 2};',
                    fixture: 'Test.expect(false, a);',
                    testFramework: 'cw-2'
                }, function(buffer) {
                    expect(buffer.stdout).to.contain('{ b: 2 }');
                    expect(buffer.stdout).to.contain('<FAILED::>');
                    done();
                });
            });
            it( 'should handle outputting objects with 0.10.33', function(done){
                // only 0.10.33 allows us to declare a without var
                runner.run({
                    language: 'javascript',
                    languageVersion: '0.10.33',
                    code: 'a = {b: 2};',
                    fixture: 'Test.expect(false, a);',
                    testFramework: 'cw-2'
                }, function(buffer) {
                    expect(buffer.stdout).to.contain('{ b: 2 }');
                    expect(buffer.stdout).to.contain('<FAILED::>');
                    done();
                });
            });

            it('should handle a basic assertion', function(done){
                runner.run({language: 'javascript', code: 'var a = 1', fixture: 'Test.expect(a == 1);', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
            });

            it('should handle comments as fixture', function(done){
                runner.run({language: 'javascript', code: 'console.log(42)', fixture: '//', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('42\n');
                    done();
                });
            });

            it('should handle a basic failed test', function(done){
                runner.run({language: 'javascript', code: 'var a = 1', fixture: 'Test.expect(a == 2)', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('<FAILED::>Value is not what was expected\n');
                    done();
                });
            });

            it('should handle logging objects', function(done){
                runner.run({language: 'javascript', code:'console.log({a: 1});', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('{ a: 1 }\n');
                    done();
                });
            });

            it('should handled nested describes', function(done) {
                runner.run({
                    language: 'javascript',
                    code: `var a = 1`,
                    fixture: `
                        describe("top", function(){
                            describe("2nd", function(){
                                it("should a", function(){
                                    Test.expect(true);
                                });
                            });
                            it("should b", function(){
                                Test.expect(true);
                            });
                        });
                    `,
                    testFramework: 'cw-2'
                }, function(buffer) {
                    expect(buffer.stdout).to.include(`<DESCRIBE::>top\n<DESCRIBE::>2nd\n<IT::>should a\n`);
                    done();
                });
            });

            describe("async handling", function() {
                it( 'should throw a timeout if code runs too long', function(done) {
                    runner.run({
                        language: 'javascript',
                        code: 'function solution() {}',
                        fixture: `
                            describe("test", 2, function(){
                                it("should do something", function(done){
                                });
                            });
                        `,
                        testFramework: 'cw-2'
                    },
                    function(buffer) {
                        expect(buffer.stdout).to.include("<ERROR::>`it` function timed out. Function ran longer than 2ms\n<COMPLETEDIN::>")
                        done();
                    });
                });

                it( 'should render in proper order', function(done) {
                    runner.run({
                        language: 'javascript',
                        code: 'function solution(cb) {setTimeout(() => cb("ok"), 0)}',
                        fixture: `
                            describe("test", true, function(){
                                it("should do something", function(done){
                                    solution((msg) => {
                                        Test.assertEquals(msg, "ok");
                                        done();
                                    });
                                    console.log("ran solution");
                                });
                            });
                        `,
                        testFramework: 'cw-2'
                    },
                    function(buffer) {
                        expect(buffer.stdout).to.include("<IT::>should do something\nran solution\n<PASSED::>Test Passed: Value == \'ok\'\n<COMPLETEDIN::>")
                        done();
                    });
                });
            });

            describe('error handling', function() {
                it( 'should handle a mix of failures and successes', function(done) {
                    runner.run({language: 'javascript',
                                code:'var a = 1',
                                fixture: 'describe("test", function(){\n' +
                                'it("test1", function(){ Test.expect(false) });' +
                                'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                                    expect(buffer.stdout).to.contain('<FAILED::>Value is not what was expected');
                                    expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                                    done();
                                });
                });
                it('should gracefully handle custom errors', function(done) {
                    runner.run({language: 'javascript',
                                code:'var a = 1',
                                fixture: 'describe("test", function(){\n' +
                                'it("test1", function(){ throw "boom!" });' +
                                'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                                    expect(buffer.stdout).to.contain('<ERROR::>');
                                    expect(buffer.stdout).to.contain('boom!');
                                    expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                                    done();
                                });
                });
                it('should gracefully handle reference errors', function(done) {
                    runner.run({language: 'javascript',
                                code:'var a = 1',
                                fixture: 'describe("test", function(){\n' +
                                'it("test1", function(){ b.test() });' +
                                'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                                    expect(buffer.stdout).to.contain('<ERROR::>');
                                    expect(buffer.stdout).to.contain('<:LF:>');
                                    expect(buffer.stdout).to.contain('ReferenceError:');
                                    expect(buffer.stdout).to.not.contain('[eval]');
                                    expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                                    done();
                                });
                });
                it('should gracefully top level handle reference errors', function(done) {
                    runner.run({language: 'javascript',
                                code:'b.test()',
                                fixture: 'describe("test", function(){\n' +
                                'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                                    expect(buffer.stdout).to.contain('<ERROR::>');
                                    expect(buffer.stdout).to.contain('<:LF:>');
                                    expect(buffer.stdout).to.contain('ReferenceError:');
                                    expect(buffer.stdout).to.not.contain('[eval]');
                                    expect(buffer.stdout).to.not.contain('Object.Test.handleError');
                                    done();
                                });
                });
            });

            it( 'should support options files', function(done){
                runner.run({
                        language: 'javascript',
                        code: 'var name = require("/workspace/config.js").name',
                        fixture: `
                        var expect = require('chai').expect;
                        describe("files", function(){
                            it("should be able to require the file", function(){
                                expect(name).to.equal("example");
                            });
                        });
                    `,
                        files: {
                            "config.js": "module.exports.name = 'example';"
                        },
                        testFramework: 'cw-2'},
                    function(buffer) {
                        expect(buffer.stdout).to.contain('<PASSED::>');
                        done();
                    });
            });
        });


        //----------------------------------------------------------------------------------------
        // Karma BDD
        //----------------------------------------------------------------------------------------

        describe('karma bdd', function() {
            it( 'warmup test', function(done){
                runner.run({
                        language: 'javascript',
                        code: 'var a = {b: 2};',
                        fixture: 'describe("test", function(){it("should be 2", function(){assert.equal(2, a.b);})});',
                        testFramework: 'karma_bdd'
                    },
                    function(){ done(); }
                );
            });
            it( 'should handle basic tests', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var a = {b: 2};',
                    fixture: 'describe("test", function(){it("should be 2", function(){assert.equal(2, a.b);})});',
                    testFramework: 'karma_bdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('<PASSED::>');
                    done();
                });
            });
            it( 'should handle loading Angular', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input * 2;
                }
        };
    });`,
                    fixture: `
describe("test", function(){
    beforeEach(module('testModule'));
    it("should multiply", inject(function(testService) {
        expect(testService).to.be.ok;
        expect(testService.double(2)).to.eql(4);
    }));
});`,
                    testFramework: 'karma_bdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<IT::>should multiply');
                    expect(buffer.stdout).to.contain('\n<PASSED::>');
                    done();
                });
            });

            it( 'should handle failures', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input * 3;
                }
        };
    });`,
                    fixture: `
describe("test", function(){
    beforeEach(module('testModule'));
    it("should multiply", inject(function(testService) {
        expect(testService.double(2)).to.eql(4);
    }));
});`,
                    testFramework: 'karma_bdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<IT::>should multiply');
                    expect(buffer.stdout).to.contain('\n<FAILED::>');
                    done();
                });
            });

            it( 'should handle code errors', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input2 * 2;
                }
        };
    });`,
                    fixture: `
describe("test", function(){
    beforeEach(module('testModule'));
    it("should multiply", inject(function(testService) {
        expect(testService.double(2)).to.eql(4);
    }));
});`,
                    testFramework: 'karma_bdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<FAILED::>');
                    done();
                });
            });

            it( 'should handle test errors', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input * 2;
                }
        };
    });`,
                    fixture: `
describe("test", function(){
    beforeEach(module('testModule'));
    it("should multiply", inject(function(testService) {
        expect(testService.double(2)).to.eql(4);
        expect(testServie.double(3)).to.eql(6);
    }));
});`,
                    testFramework: 'karma_bdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<FAILED::>');
                    done();
                });
            });
        });


        //----------------------------------------------------------------------------------------
        // Karma TDD
        //----------------------------------------------------------------------------------------

        describe('karma tdd', function() {
            it( 'should handle basic tests', function(done){
                runner.run({
                    language: 'javascript',
                    code: 'var a = {b: 2};',
                    fixture: 'suite("test", function(){test("should be 2", function(){assert.equal(2, a.b);})});',
                    testFramework: 'karma_tdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('<PASSED::>');
                    done();
                });
            });
            it( 'should handle loading Angular', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input * 2;
                }
        };
    });`,
                    fixture: `
suite("test", function(){
    setup(module('testModule'));
    test("should multiply", inject(function(testService) {
        expect(testService).to.be.ok;
        expect(testService.double(2)).to.eql(4);
    }));
});`,
                    testFramework: 'karma_tdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<IT::>should multiply');
                    expect(buffer.stdout).to.contain('\n<PASSED::>');
                    done();
                });
            });

            it( 'should handle failures', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input * 3;
                }
        };
    });`,
                    fixture: `
suite("test", function(){
    setup(module('testModule'));
    test("should multiply", inject(function(testService) {
        expect(testService.double(2)).to.eql(4);
    }));
});`,
                    testFramework: 'karma_tdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<IT::>should multiply');
                    expect(buffer.stdout).to.contain('\n<FAILED::>');
                    done();
                });
            });

            it( 'should handle code errors', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input2 * 2;
                }
        };
    });`,
                    fixture: `
suite("test", function(){
    setup(module('testModule'));
    test("should multiply", inject(function(testService) {
        expect(testService.double(2)).to.eql(4);
    }));
});`,
                    testFramework: 'karma_tdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<FAILED::>');
                    done();
                });
            });

            it( 'should handle test errors', function(done){
                runner.run({
                    language: 'javascript',
                    setup: '// @include-external angular@1.5',
                    code: `
angular.module('testModule', [])
    .factory('testService', function() {
        return {
                double(input) {
                    return input * 2;
                }
        };
    });`,
                    fixture: `
suite("test", function(){
    setup(module('testModule'));
    test("should multiply", inject(function(testService) {
        expect(testService.double(2)).to.eql(4);
        expect(testServie.double(3)).to.eql(6);
    }));
});`,
                    testFramework: 'karma_tdd'
                },
                function(buffer) {
                    expect(buffer.stdout).to.contain('\n<FAILED::>');
                    done();
                });
            });
        });
    });
});
