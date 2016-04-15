var expect = require('chai').expect;
var runner = require('../runner');


describe( 'typescript runner', function(){
    //describe( '.run', function(){
    //    it( 'should handle basic code evaluation', function(done){
    //        runner.run({language: 'typescript', code: 'console.log(42)'}, function(buffer) {
    //            expect(buffer.stdout).to.equal('42\n');
    //            done();
    //        });
    //    });
    //});

    describe('mocha bdd', function() {
        it( 'should handle outputting objects', function(done){
            runner.run({
                    language: 'typescript',
                    code: 'interface B { b:number }; var a:B = {b: 3};exports.a = a;',
                    fixture: 'var assert = require("chai").assert;describe("test", function(){it("should be 3", function(){assert.equal(3, solution.a.b);})});',
                    testFramework: 'mocha_bdd'},
                function(buffer) {
                    expect(buffer.stdout).to.contain('<PASSED::>');
                    done();
                });
        });
        it( 'should handle failures', function(done){
            runner.run({
                    language: 'typescript',
                    code: 'exports.a = {b: 2};',
                    fixture: 'var assert = require("chai").assert;describe("test", function(){describe("failures", function(){it("should be 1", function(){assert.equal(1, solution.a.b);})})});',
                    testFramework: 'mocha_bdd'},
                function(buffer) {
                    expect(buffer.stdout).to.contain('<FAILED::>');
                    done();
                });
        });
        it( 'should handle errors', function(done){
            runner.run({
                    language: 'typescript',
                    code: 'exports.a = {b: 2};',
                    fixture: 'describe("test", function(){describe("failures", function(){it("should be 1", function(){throw new Error("test error");})})});',
                    testFramework: 'mocha_bdd'},
                function(buffer) {
                    expect(buffer.stdout).to.contain('<ERROR::>');
                    done();
                });
        });
    });
});
