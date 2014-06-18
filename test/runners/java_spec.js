var expect = require('chai').expect;
var runner = require('../../lib/runners/java');


describe( 'java runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'java',
                        solution: 'class Solution {\n'
                                + '    public static void main(String[] args){\n'
                                + '        System.out.println("42");\n'
                                + '    }\n'
                                + '}\n'
                    }, function(buffer) {
                expect(buffer.stdout ).to.equal('42\n');
                done();
            });
        });
    });
});
