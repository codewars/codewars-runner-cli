var expect = require('chai').expect;
var runner = require('../../lib/runners/go');

describe( 'go runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            var solution = ['package main',
                            'import "fmt"',
                            'func main() {',
                            '    fmt.Println("42")',
                            '}',
                            ''].join('\n');

            runner.run({language: 'go', solution: solution}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});