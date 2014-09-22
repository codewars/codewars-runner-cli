var expect = require('chai').expect;
var runner = require('../../lib/runners/objc');


describe( 'objc runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({
		    language: 'objc', 
		    solution: [
		    "#import <stdio.h>",
                    "#import <Foundation/Foundation.h>",
		    "int main(void)",
		    "{",
			"NSLog(@\"Object-oriented programming is an exceptionally bad idea which could only have originated in California. - Edsger Dijkstra\");",
                        "return 0;",
	            "}"
		    ].join('\n')
	    }, function(buffer) {
		console.log(buffer);
                expect(buffer.stderr).to.contain("Object-oriented programming is an exceptionally bad idea which could only have originated in California. - Edsger Dijkstra\n");
                done();
            });
        });
    });
});
