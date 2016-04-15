if (process.platform == 'darwin') {
    var expect = require('chai').expect;
    var runner = require('../runner');

    describe( 'objc runner', function(){
	describe( '.run', function(){
            it( 'should handle basic code evaluation', function(done){
		runner.run({
		    language: 'objc', 
		    code: [
			'#import <stdio.h>',
			'#import <Foundation/Foundation.h>',
			'int main(void)',
			'{',
			'NSLog(@"Object-oriented programming is an exceptionally bad idea which could only have originated in California. - Edsger Dijkstra");',
                        'return 0;',
			'}'
		    ].join('\n')
		}, function(buffer) {
		    console.log(buffer);
                    expect(buffer.stderr).to.contain('Object-oriented programming is an exceptionally bad idea which could only have originated in California. - Edsger Dijkstra\n');
                    done();
		});
            });
            it('should handle basic code setup code', function(done) {
		runner.run({
		    language: 'objc', 
		    setup: 'int foo(void) { return 999; }',
		    setupHeader: [
                        '//',
                        '// test_setup.h',
                        'int foo(void);'
		    ].join('\n'),
		    code: [
			'#import <stdio.h>',
			'#import <Foundation/Foundation.h>',
			'#import "test_setup.h"',
			'int main(void)',
			'{',
			'NSLog(@"A string: %i", foo());',
                        'return 0;',
			'}'
		    ].join('\n')
		}, function(buffer) {
		    console.log(buffer);
                    expect(buffer.stderr).to.contain('999');
                    done();
		});
            });
            it('should handle broken fixture', function(done) {
		runner.run({
		    language: 'objc', 
		    code: 'int flah(void) { return 9999; }',
		    codeHeader: [
			'// code.h',
			'int flah(void);'
		    ].join('\n'),
		    fixture: [
			'#import <XCTest/XCTest.h>',
			'@interface MyAppTests : XCTestCase',
			'@end',
			'@implementation MyAppTests',
			'- (void)testXXXX',
			'{',
			   'XCTAssert(1)',
			'}',
			'@end'
                    ].join('\n')


		}, function(buffer) {
		    console.log(buffer);
                    expect(buffer.stdout).to.contain('test.m:7:13: error: expected');
                    done();
		});
            });
            it('should handle test fixture', function(done) {
		runner.run({
		    language: 'objc', 
		    code: 'float flah(void) { return 9999; }',
		    codeHeader: [
			'// code.h',
			'float flah(void);'
		    ].join('\n'),
		    fixture: [
			'#import <XCTest/XCTest.h>',
			'#include "code.h"',
			'@interface MyAppTests : XCTestCase',
			'@end',
			'@implementation MyAppTests',
			'- (void)testXXXX',
			'{',
			   'XCTAssert(flah() == 9999);',
			'}',
			'@end'
                    ].join('\n')


		}, function(buffer) {
		    console.log(buffer);
                    expect(buffer.stdout).to.contain("<PASSED::>Test Case '-[MyAppTests testXXXX]' passed");
                    done();
		});
            });
            it('should handle test fixture with setup', function(done) {
		runner.run({
		    language: 'objc', 
                    setup: [
			'float bar(void) { return 30.75; }'
		    ].join('\n'),
                    setupHeader: [
			'// bar.h',
			'float bar(void);'
		    ].join('\n'),
                    code: [
			'#include "bar.h"',
			'float foo(void) { return bar(); }'
		    ].join('\n'),
                    codeHeader: [
			'// payload.h',
			'float foo(void);'
		    ].join('\n'),
		    fixture: [
			'#import <XCTest/XCTest.h>',
			'#include "payload.h"',
			'@interface FooOBJCTests : XCTestCase',
			'@end',
			'@implementation FooOBJCTests',
			'- (void)testFOO',
			'{',
			   'XCTAssertEqual(foo(), 30.75);',
			'}',
			'@end'
                    ].join('\n')


		}, function(buffer) {
		    console.log(buffer);
                    expect(buffer.stdout).to.contain("<PASSED::>Test Case '-[FooOBJCTests testFOO]' passed");
                    done();
		});
            });

	});
    });
}
