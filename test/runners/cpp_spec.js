var expect = require('chai').expect;
var runner = require('../../lib/runners/cpp');

describe( 'c++ runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            var solution = ['#include <iostream>',
                            'int main()',
                            '{ std::cout << "\\\"Within C++, there is a much smaller and cleaner language struggling to get out.\\\" - Bjarn Stroustrup"; }'
                            ].join('\n');

            runner.run({language: 'c++', solution: solution}, function(buffer) {
                expect(buffer.stdout).to.equal("\"Within C++, there is a much smaller and cleaner language struggling to get out.\" - Bjarn Stroustrup");
                done();
            });
        });
    });
});
