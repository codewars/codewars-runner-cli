var expect = require('chai').expect;
var runner = require('../runner');

describe('objc runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('objc');

    it('should handle basic code evaluation', function(done) {
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
        expect(buffer.stdout).to.contain('Hello World\n');
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
        //console.log(buffer);
        expect(buffer.stdout).to.contain('999');
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
        code: `
                    #import <Foundation/Foundation.h>
                        int main (int argc, const char * argv[]) {
                        NSLog(@"A string: %i", foo(noexists));
                        return 0;
                    }
                    `
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stderr).to.contain("error: use of undeclared identifier 'noexists'");
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
        //console.log(buffer);
        expect(buffer.stdout).to.contain('Square: 36');
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
        code:  `
	        int main (int argc, const char * argv[]) {
            #if !__has_feature(objc_arc)
              // Manual memory management
              NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
            #else
              // ARC enabled, do nothing...
            #endif
            
            BankAccount *account1, *account2;
            account1 = [[BankAccount newAlloc] init];
            account2 = [[BankAccount newAlloc] init];
            int count = [BankAccount totalOpen];
            NSLog (@"Number of BankAccount instances = %i", count);

            #if !__has_feature(objc_arc)
              // Manual memory management
              [account1 release];
              [account2 release];
              [pool drain];
            #else
              // ARC enabled, do nothing...
            #endif

            return 0;
        }
	    `
      }, function(buffer) {
        //console.log(buffer);
        console.log("buffer.stderr", buffer.stderr.split("\n"));
        expect(buffer.stdout).to.contain('Number of BankAccount instances = 2');
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
        code:  `
					int main (int argc, const char * argv[]) {
					#if !__has_feature(objc_arc)
				    // Manual memory management
				    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
					#else
					  // ARC enabled, do nothing...
					#endif

					  SimpleClass *simple = [[SimpleClass alloc] init];
					  simple.name = @"Codewars";
					  simple.age = 108;
					  [simple printName];
					  
					#if !__has_feature(objc_arc)
					  // Manual memory management
					  [simple release];
					  [pool drain];
					#else
					  // ARC enabled, do nothing...
					#endif                        

					return 0;
					}
				`
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stdout).to.contain('Name: Codewars and the age is 108');
        done();
      });
    });
    it('should handle unhandled exceptions', function(done) {
      runner.run({
        language: 'objc',
        testFramework: 'unitkit',
        code: `
                    #import <Foundation/Foundation.h>

                    NSString* FooException (NSString *str){
                        [NSException raise:@"FooException" format:@"Custom exception"];
                        return str;
                    }
                `,
        fixture: `
                    @implementation TestSuite

                    - (void) testsFooUnhandledException
                    {
                        UKStringsEqual(@"Blah", FooException(@"Blah"));
                    }

                    @end
                `
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>TestSuite');
        expect(buffer.stdout).to.contain('<IT::>testsFooUnhandledException');
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.contain('NSException: FooException Custom exception');
        expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
        done();
      });
    });
    it('should handle functions from standard library <math.h>', function(done) {
      runner.run({
        language: 'objc',
        setup: false,
        code:[
          '#import <Foundation/Foundation.h>',
          '#include <math.h>',
          'int main (int argc, const char * argv[]) {',
          '@autoreleasepool{',
          'NSLog(@"%.f", sqrt(pow(5, 2)) );',
          '}',
          'return 0;',
          '}'
        ].join('\n')
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stdout).to.contain('5');
        done();
      });
    });
    it('should support modern objc', function(done) {
      runner.run({
        language: 'objc',
        setup: false,
        code:`
                    #import <Foundation/Foundation.h>

                    NSNumber *ICKGetMaxProfit(NSArray<NSNumber *> *stockPricesYesterday, NSUInteger length) {
                        NSInteger minPrice, maxProfit;

                        NSCAssert1(length >= 2,
                            @"parameter length: expected 2 or more but got %lu", (unsigned long)length);

                        minPrice = stockPricesYesterday[0].integerValue;
                        maxProfit = stockPricesYesterday[1].integerValue - stockPricesYesterday[0].integerValue;

                        for (NSUInteger i = 1; i < length; i++) {
                            NSInteger currentPrice = stockPricesYesterday[i].integerValue;
                            NSInteger potentialProfit = currentPrice - minPrice;

                            maxProfit = MAX(maxProfit, potentialProfit);
                            minPrice = MIN(minPrice, currentPrice);
                        }

                        return @(maxProfit);
                    }


                    int main (int argc, const char * argv[]) {
                        NSArray *stockPricesYesterday = @[ @15, @20, @19];
                        NSLog(@"%@", ICKGetMaxProfit(stockPricesYesterday,3));
                        return 0;
                    }`
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.stdout).to.contain('5');
        done();
      });
    });
    it('should get the return code', function(done) {
      var solution = `
                int main (int argc, const char * argv[]) {
                    return 10;
                }
            `;
      runner.run({
        language: 'objc',
        code: solution
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.exitCode).to.equal(10);
        expect(buffer.exitSignal).to.equal(null);
        done();
      });
    });
    it('should catch signals on crash', function(done) {
      var solution = `
                #import <Foundation/Foundation.h>
                int main (int argc, const char * argv[]) {
                      int *nullPointer = nil;
                      *nullPointer = 0;
                }
            `;
      runner.run({
        language: 'objc',
        code: solution
      }, function(buffer) {
        //console.log(buffer);
        expect(buffer.exitCode).to.equal(null);
        expect(buffer.exitSignal).to.equal('SIGSEGV');
        done();
      });
    });
    it('should not support manual memory management with ARC', function(done) {
      runner.run({
        language: 'objc',
        languageVersion: 'objc-arc',
        setup: false,
        code:`
        #import <Foundation/Foundation.h>
          int main (int argc, const char * argv[]) {
            NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
            NSLog(@"Hello World");
            [pool drain];
          return 0;
          }`
      }, function(buffer) {
        expect(buffer.stderr).to.contain("error: 'NSAutoreleasePool' is unavailable: Not available with automatic reference counting");
        done();
      });
    });

    it('should comply about blocks as not id, when block is not casted', function(done) {
      runner.run({
        language: 'objc',
        setup: false,
        code:`
            #import <Foundation/Foundation.h>
            NSMutableArray<int(^)(void)> * initBlocksArray(){ 

              return [ NSMutableArray arrayWithObjects:^{ return 100; }, ^{ return 200; },nil ];
            }            

            int main (int argc, const char * argv[]) {
              @autoreleasepool{
                NSMutableArray<int(^)(void)> * blocks = initBlocksArray();
                NSLog(@"%d", blocks[1]() );
                return 0;
              }
            }`
      }, function(buffer) {
        expect(buffer.stderr).to.contain("error: called object type 'id' is not a function or function pointer");
        done();
      });
    });
    it('should support casted blocks', function(done) {
      runner.run({
        language: 'objc',
        setup: false,
        code:`
            #import <Foundation/Foundation.h>

            NSMutableArray<int(^)(void)> * initBlocksArray(){ 

              int(^blockOne)(void) = ^{ return 100; };  // this is a global block
              int(^blockTwo)(void) = ^{ return 200; };  // this is a global block
              int(^blockThree)(void) = ^{ return blockTwo(); };  // this is a stack block, it SHOULD be copied if assigned

              NSLog(@"three is: %@", blockThree );

              NSMutableArray *array = [NSMutableArray arrayWithObjects:blockOne, [[blockThree copy] autorelease], nil];

              return array;
            }            

            int main (int argc, const char * argv[]) {
              @autoreleasepool{
                NSMutableArray<int(^)(void)> * blocks = initBlocksArray();

                NSLog(@"%d", ((int(^)(void))blocks[1])() );
                return 0;
              }
            }`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('three is: <_NSConcreteStackBlock');
        expect(buffer.stdout).to.contain('200');
        done();
      });
    });
    it('should support blocks with ARC', function(done) {
      runner.run({
        language: 'objc',
        languageVersion: 'objc-arc',
        setup: false,
        code:`
            #import <Foundation/Foundation.h>

            NSMutableArray<int(^)(void)> * initBlocksArray(){ 

              int(^blockOne)(void) = ^{ return 100; };  // this is a global block
              int(^blockTwo)(void) = ^{ return 200; };  // this is a global block
              int(^blockThree)(void) = ^{ return blockTwo(); };  // with ARC this is a malloc block

              NSLog(@"three is: %@", blockThree );

              NSMutableArray *array = [NSMutableArray arrayWithObjects:blockOne, blockThree, nil];

              return array;
            }           

            int main (int argc, const char * argv[]) {
              @autoreleasepool{
                NSMutableArray<int(^)(void)> * blocks = initBlocksArray();

                NSLog(@"result: %d", ((int(^)(void))blocks[1])() );
                return 0;
              }
            }`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('three is: <_NSConcreteMallocBlock');
        expect(buffer.stdout).to.contain('result: 200');
        done();
      });
    });
    it('should support blocks without cast on NSArray', function(done) {
      runner.run({
        language: 'objc',
        languageVersion: 'objc-arc',
        setup: false,
        code:`
            #import <Foundation/Foundation.h>

            NSArray<int(^)(void)> * initBlocksArray(){ 

              int(^blockOne)(void) = ^{ return 100; };  // this is a global block
              int(^blockTwo)(void) = ^{ return 200; };  // this is a global block
              int(^blockThree)(void) = ^{ return blockTwo(); };  // with ARC this is a malloc block

              NSLog(@"three is: %@", blockThree );

              return @[blockOne, blockThree];              
            }           

            int main (int argc, const char * argv[]) {
              @autoreleasepool{
                NSArray<int(^)(void)> * blocks = initBlocksArray();

                NSLog(@"result: %d",blocks[1]() );
                return 0;
              }
            }`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('three is: <_NSConcreteMallocBlock');
        expect(buffer.stdout).to.contain('result: 200');
        done();
      });
    });
    it('should support blocks without cast on NSArray', function(done) {
      runner.run({
        language: 'objc',
        setup: false,
        code:`
            #import <Foundation/Foundation.h>

            NSArray<int(^)(void)> * initBlocksArray(){ 

              int(^blockOne)(void) = ^{ return 100; };  // this is a global block
              int(^blockTwo)(void) = ^{ return 200; };  // this is a global block
              int(^blockThree)(void) = ^{ return blockTwo(); };  // this is a stack block, it SHOULD be copied if assigned

              NSLog(@"three is: %@", blockThree );

              return @[blockOne, [[blockThree copy] autorelease]];              
            }           

            int main (int argc, const char * argv[]) {
              @autoreleasepool{
                NSArray<int(^)(void)> * blocks = initBlocksArray();

                NSLog(@"result: %d",blocks[1]() );
                return 0;
              }
            }`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('three is: <_NSConcreteStackBlock');
        expect(buffer.stdout).to.contain('result: 200');
        done();
      });
    });

    describe('UnitKit', function() {
      it('should perform unit testing', function(done) {
        runner.run({
          language: 'objc',
          code: ' ',
          testFramework: 'unitkit',
          fixture: `
                        @implementation TestSuite

                        + (void)testAClassMethod
                        {
                           UKPass();
                        }

                        - (void)testIfPass
                        {
                            UKPass();
                        }

                        @end
                    `
        }, function(buffer) {
          //console.log(buffer);
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestSuite');
          expect(buffer.stdout).to.contain('<IT::>testAClassMethod');
          expect(buffer.stdout).to.contain('<IT::>testIfPass');
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.not.contain('<FAILED::>');
          expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
          done();
        });
      });
      it('should handling failures', function(done) {
        runner.run({
          language: 'objc',
          code: ' ',
          testFramework: 'unitkit',
          fixture: `
                        @implementation TestSuite

                        - (void) testsFailures
                        {
                            UKFail();
                            UKTrue(NO);
                            UKFalse(YES);
                            UKNil(@"fwip");
                            UKNil(self);
                            UKNotNil(nil);
                            UKIntsEqual(3, 4);
                            UKIntsNotEqual(3, 3);
                            UKFloatsEqual(22.0, 33.0, 1.0);
                            UKFloatsNotEqual(22.1, 22.2, 0.2);
                            UKObjectsEqual(self, @"foo");
                            UKObjectsNotEqual(self, self);
                            UKObjectsSame(self, @"foo");
                            UKObjectsNotSame(@"foo", @"foo");
                            UKStringsEqual(@"fraggle", @"rock");
                            UKStringsNotEqual(@"fraggle", @"fraggle");
                            UKStringContains(@"fraggle", @"no");
                            UKStringDoesNotContain(@"fraggle", @"fra");
                        }

                        @end
                    `
        }, function(buffer) {
          //console.log(buffer);
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestSuite');
          expect(buffer.stdout).to.contain('<IT::>testsFailures');
          expect(buffer.stdout).to.not.contain('<PASSED::>');
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
          done();
        });
      });
      it('should unit test with code', function(done) {
        runner.run({
          language: 'objc',
          testFramework: 'unitkit',
          code: `
                        #import <Foundation/Foundation.h>

                        NSString* Foo (NSString *str){return str;}
                    `,
          fixture: `
                        @implementation TestSuite

                        - (void) testsFoo
                        {
                            UKStringsEqual(@"Blah", Foo(@"Blah"));
                            UKStringsNotEqual(@"fraggle", Foo(@"Blah"));
                        }

                        @end
                    `
        }, function(buffer) {
          //console.log(buffer);
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestSuite');
          expect(buffer.stdout).to.contain('<IT::>testsFoo');
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.not.contain('<FAILED::>');
          expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
          done();
        });
      });
      it('should handling exceptions', function(done) {
        runner.run({
          language: 'objc',
          testFramework: 'unitkit',
          code: `
                        #import <Foundation/Foundation.h>

                        NSString* FooException (NSString *str){
                            [NSException raise:@"FooException" format:@"Custom exception"];
                            return str;
                        }
                    `,
          fixture: `
                        @implementation TestSuite

                        - (void) testsFooException
                        {
                            UKRaisesException(FooException(@"Blah"));
                        }

                        @end
                    `
        }, function(buffer) {
          //console.log(buffer);
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestSuite');
          expect(buffer.stdout).to.contain('<IT::>testsFooException');
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.not.contain('<FAILED::>');
          expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
          done();
        });
      });

      it('should support setup code', function(done) {
        // https://github.com/Codewars/codewars.com/issues/1221
        runner.run({
          language: 'objc',
          testFramework: 'unitkit',
          setup: `
#import <Foundation/Foundation.h>

@interface Node: NSObject
{
  int data;
  Node *next;
}
@property (readonly) int data;
@property (readonly) Node *next;
- (id)initWithData: (int)d andNext: (Node *)n;
- (id)initWithData: (int)d;
+ (Node *)nodeWithData: (int)d andNext: (Node *)n;
+ (Node *)nodeWithData: (int)d;
@end

@implementation Node
@synthesize data;
@synthesize next;
- (id)initWithData: (int)d andNext: (Node *)n
{
  data = d;
  next = n;
  return self;
}
- (id)initWithData: (int)d
{
  data = d;
  next = NULL;
  return self;
}
+ (Node *)nodeWithData: (int)d andNext: (Node *)n
{
  return [[Node alloc] initWithData: d andNext: n];
}
+ (Node *)nodeWithData: (int)d
{
  return [[Node alloc] initWithData: d];
}
@end
          `,
          solution: `
#import <Foundation/Foundation.h>

NSString *stringify(Node *list) {
  // TODO: Return a string representation of the list passed in
  return @"";
}
          `,
          fixture: `
@implementation TestSuite
- (void)testNULL
{
  UKStringsEqual(@"", stringify(NULL));
}
- (void)testSingle
{
  UKStringsEqual(@"1", stringify([Node nodeWithData: 1]));
}
@end
          `,
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.contain('<FAILED::>');
          done();
        });
      });
    });

    describe('CW', function() {
      it('should perform unit testing and test for failure (test equal())', function(done) {
        runner.run({
          language: 'objc',
          code: 'NSString* Foo (NSString *str){return str;}',
          testFramework: 'cw',
          fixture: `
                        describe(@"String match", ^() {
                            it(@"should not match", ^() {
                                equal(@"Blah", Foo(@"Blah1"));
                            });
                        });
                    `
        }, function(buffer) {
          console.log(buffer);
          expect(buffer.stdout).to.contain('<DESCRIBE::>String match');
          expect(buffer.stdout).to.contain('<IT::>should not match');
          expect(buffer.stdout).to.contain('<FAILED::>Expected "Blah" (NSConstantString) but instead got "Blah1" (NSConstantString)');
          expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
          done();
        });
      });
      it('should perform unit testing and pass the test (test notEqual())', function(done) {
        runner.run({
          language: 'objc',
          code: 'NSString* Foo (NSString *str){return str;}',
          testFramework: 'cw',
          fixture: [
            'describe(@"String not match", ^() {',
            'it(@"should pass", ^() {',
            'notEqual(@"Blah", Foo(@"Blah"));',
            '});',
            '});'
          ].join('\n')
        }, function(buffer) {
          console.log(buffer);
          expect(buffer.stdout).to.contain('<FAILED::>Value is not supposed to equal "Blah" (NSConstantString)');
          expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
          expect(buffer.stdout).to.contain('<IT::>should pass');
          expect(buffer.stdout).to.contain('<DESCRIBE::>String not match');
          expect(buffer.stderr).to.equal('');
          done();
        });
      });
    });
  });
});
