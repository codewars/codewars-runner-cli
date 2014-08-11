var expect = require('chai').expect;
var runner = require('../../lib/runners/c');

describe( 'c runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            var solution = ['#include <stdio.h>',
                            'int main() {',
                            '    printf("I think the next language codewars should support is PL/I.  Wikipedia says it\'s still used in production to this day; presumably in nursing homes?");',
                            '}',
                            ''].join('\n');

            runner.run({language: 'c', solution: solution}, function(buffer) {
                expect(buffer.stdout).to.equal("I think the next language codewars should support is PL/I.  Wikipedia says it\'s still used in production to this day; presumably in nursing homes?");
                done();
            });
        });
    });
});