
var expect = require('chai').expect;
var process = require('../lib/opts').process;

var opts;

describe( 'opts', function(){
    describe( '.process', function(){
        describe( 'language arg', function(){
            describe( 'when valid args are given', function(){
                before( function(done){
                    process({language: 'js', solution: '1+1', fixture: 'expect(1).to.equal(1)'}, function(o){
                        opts = o;
                        done();
                    });
                });

                it( 'should have returned options', function(){
                    expect(opts).to.not.equal(null)
                });

                it( 'should convert abbreviated name to full name', function(){
                    expect(opts.language).to.equal('javascript');
                });

                it( 'should set testFramework to default if it was missing', function(){
                    expect(opts.testFramework).to.equal('cw-2');
                });
            });

        });

        describe( 'solutionFile arg', function() {
            before(function(done){
                process({language: 'js', solutionFile: './test/support/solution.js', fixture: 'expect(1).to.equal(1)'}, function(o){
                    opts = o;
                    done();
                });
            });

            it( 'should read contents into solution option', function() {
                expect(opts.solution ).to.equal('a = 1;');
            });
        });
    });
});