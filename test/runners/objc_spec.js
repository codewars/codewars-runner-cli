var expect = require('chai').expect;
var runner = require('../runner');

describe( 'objc runner', function(){
	describe( '.run', function(){
		it( 'should handle basic code evaluation', function(done){
			runner.run({
				language: 'objc',
                setup: false,
				code: [
                    '#import <Foundation/Foundation.h>',
                    'int main (int argc, const char * argv[]) {',
					'NSLog(@"Hello World");',
                    'return 0;',
                    '}'
				].join('\n')
			}, function(buffer) {
				console.log("buffer", buffer);
				expect(buffer.stderr).to.contain('Hello World\n');
				done();
			});
		});
		it('should handle basic code setup code', function(done) {
			runner.run({
				language: 'objc',
				setup: 'int foo(void) { return 999; }',
				setupHeader: [
					'int foo(void);'
				].join('\n'),
				code: [
                    '#import <Foundation/Foundation.h>',
                    'int main (int argc, const char * argv[]) {',
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
		it('should handle compile errors', function(done) {
			runner.run({
				language: 'objc',
				setup: 'int foo(void) { return 999; }',
				setupHeader: [
					'int foo(void);'
				].join('\n'),
				code: [
                    '#import <Foundation/Foundation.h>',
                    'int main (int argc, const char * argv[]) {',
                    'NSLog(@"A string: %i", noexists());',
                    'return 0;',
                    '}'
				].join('\n')
			}, function(buffer) {
				console.log(buffer);
				expect(buffer.stderr).to.contain('warning: implicit declaration of function \'noexists\'');
				done();
			});
		});
		it('should handle setup code and imports', function(done) {
			runner.run({
				language: 'objc',
				setup: 'int square(int a) { return a * a; }',
				setupHeader: [
					'int square(int a);'
				].join('\n'),
				code: [
                    '#import <Foundation/Foundation.h>',
                    'int main (int argc, const char * argv[]) {',
                    'NSLog(@"Square: %i", square(6));',
                    'return 0;',
                    '}'
				].join('\n')
			}, function(buffer) {
				console.log(buffer);
				expect(buffer.stderr).to.contain('Square: 36');
				done();
			});
		});
		it('should handle constructor classes, member functions and static', function(done) {
			runner.run({
				language: 'objc',
				setup: [
                    'static int openAccounts = 0;',
                    '@implementation BankAccount',
                    '+ (BankAccount *) newAlloc {',
                    'openAccounts++;',
                    'return [BankAccount alloc];',
                    '}',
                    '+ (int) totalOpen {',
                    'return openAccounts;',
                    '}',
                    '@end'
                ].join('\n'),
				setupHeader: [
                    '#import <Foundation/Foundation.h>',
                    '@interface BankAccount: NSObject {}',
                    '+ (BankAccount *) newAlloc;',
                    '+ (int) totalOpen;',
                    '@end'
				].join('\n'),
				code: [
                    'int main (int argc, const char * argv[]) {',
                    'NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];',
                    'BankAccount *account1, *account2;',
                    'account1 = [[BankAccount newAlloc] init];',
                    'account2 = [[BankAccount newAlloc] init];',
                    'int count = [BankAccount totalOpen];',
                    'NSLog (@"Number of BankAccount instances = %i", count);',
                    '[account1 release];',
                    '[account2 release];',
                    '[pool drain];',
                    'return 0;',
                    '}'
				].join('\n')
			}, function(buffer) {
				console.log(buffer);
                console.log("buffer.stderr", buffer.stderr.split("\n"));
                expect(buffer.stderr).to.contain('Number of BankAccount instances = 2');
				done();
			});
		});
		it('should handle constructor classes, member functions and instance properties', function(done) {
			runner.run({
				language: 'objc',
				setup: [
                    '@implementation SimpleClass',
                    '@synthesize name;',
                    '@synthesize age;',
                    '- (void)printName {',
                        'NSLog(@"Name: %@ and the age is %d", name, age);',
                    '}',
                    '@end'
                ].join('\n'),
				setupHeader: [
                    '#import <Foundation/Foundation.h>',
                    '@interface SimpleClass : NSObject {',
                        'NSString* name;',
                        'int age;',
                    '}',
                    '@property NSString* name;',
                    '@property int age;',
                    '- (void)printName;',
                    '@end'
				].join('\n'),
				code: [
                    'int main (int argc, const char * argv[]) {',
                        'NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];',
                        'SimpleClass *simple = [[SimpleClass alloc] init];',
                        'simple.name = @"Codewars";',
                        'simple.age = 108;',
                        '[simple printName];',
                        '[simple release];',
                        '[pool drain];',
                    'return 0;',
                    '}'
				].join('\n')
			}, function(buffer) {
				console.log(buffer);
                console.log("buffer.stderr", buffer.stderr.split("\n"));
                expect(buffer.stderr).to.contain('Name: Codewars and the age is 108');
				done();
			});
		});
	});
});