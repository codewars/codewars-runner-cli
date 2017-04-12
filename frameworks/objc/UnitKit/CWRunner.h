/**
 * CWRunner.h
 * A test runner wrapper for UnitKit/Codewars
 *
 * Created by  Ivano Picco <ivano.picco@pianobit.com>
 *
 **/

#import <Foundation/Foundation.h>
#import <UnitKit/UnitKit.h>

#ifndef __has_feature         // Optional of course.
  #define __has_feature(x) 0  // Compatibility with non-clang compilers.
#endif
#ifndef __has_extension
  #define __has_extension __has_feature // Compatibility with pre-3.0 compilers.
#endif

#ifndef CWRunner_h
#define CWRunner_h
@interface CWRunner: NSObject
{
	UKRunner *runner;
	UKTestHandler *handler;
	NSDate *startTestDate;
}
	@property int testsPassed;
	@property int testsFailed;
	@property int exceptionsReported;
@end

@implementation CWRunner
- (instancetype)init
{
    self = [super init];
    if (self == nil)
        return nil;

	runner = [UKRunner new];
	[[UKTestHandler handler] setQuiet: NO];
	handler = [UKTestHandler handler];
	handler.delegate = self;

	_testsPassed = 0;
	_testsFailed = 0;
	_exceptionsReported = 0;

    return self;
}

- (void) dealloc {
	#if !__has_feature(objc_arc)
	    // Manual memory management
		[runner release];
		[super dealloc];
	#else
	    // ARC enabled, do nothing...
	#endif
}

- (void)reportStatus: (BOOL)cond
              inFile: (char *)filename
                line: (int)line
             message: (NSString *)msg
{
	if (cond)
  {
      _testsPassed++;
      NSLog(@"<PASSED::>Test Passed\n");
  }
  else
  {
      _testsFailed++;
      NSString *message = [msg stringByReplacingOccurrencesOfString:@"\n" withString:@"<:LF:>"];
      NSLog(@"<FAILED::>%s:%i:%s\n", filename, line, message.UTF8String);
  }
}

- (void)reportWarning: (NSString *)message
{
	_testsFailed++;
	_exceptionsReported++;
    message = [message stringByReplacingOccurrencesOfString:@"\n" withString:@"<:LF:>"];
    NSLog(@"<FAILED::>%s\n", message.UTF8String);
}

- (void)runTests: (NSArray *)testMethods onInstance: (BOOL)instance ofClass: (Class)testClass
{
	Class cls = object_getClass(runner);
	Ivar _ivarReleasing = class_getInstanceVariable(cls, "_releasing");
	id _releasing;

	//selecting hidden method
	SEL aSelector = NSSelectorFromString(@"runTestNamed:onInstance:ofClass:");
	id (*runTestNamed)(id, SEL, NSString *, BOOL, Class);

	if ( [runner respondsToSelector:aSelector] ) {
		// Get the hidden method, returns the IMP
		runTestNamed = (void *)[runner methodForSelector:aSelector];
	}  else {
		// method doesn't exits, throw an error!
		[NSException raise:@"CWRunnerException" format:@"runTestNamed not found"];
	}

    for (NSString *testMethodName in testMethods)
    {
    	NSLog(@"<IT::>%s",testMethodName.UTF8String);
    	startTestDate = [NSDate date];
    	_releasing= object_getIvar(runner,_ivarReleasing);
        @try
        {
            @autoreleasepool
            {
				// runTestNamed is just a C function, so we can call it directly.
				id returnValue = runTestNamed(runner, aSelector, testMethodName, instance, testClass);
            }
        }
        @catch (NSException *exception)
        {
            id hint = (_releasing ? @"errExceptionOnRelease" : nil);

            [[UKTestHandler handler] reportException: exception
                                             inClass: testClass
                                                hint: hint];
        }

        object_setIvar(runner,_ivarReleasing, 0);
        NSLog(@"<COMPLETEDIN::>%d", (int)([[NSDate date] timeIntervalSinceDate: startTestDate] * 1000));
    }
}

- (void)runSuite: (Class)testClass
{
    NSLog(@"<DESCRIBE::>%s", class_getName(testClass));
    NSDate *startSuiteDate = [NSDate date];

	//[runner runTestsInBundle: [NSBundle mainBundle]];
	//[runner runTestsWithClassNames: @[@"TestSuite"] principalClass:[testSuite class]];
	//[runner runTestsInClass: [testSuite class]];

    //selecting hidden method
    SEL aSelector = NSSelectorFromString(@"filterTestMethodNames:");
    NSArray * (*filterTestMethodNames)(id, SEL, NSArray *);

	if ( [runner respondsToSelector:aSelector] ) {
		// Get the hidden method, returns the IMP
		filterTestMethodNames = (void *)[runner methodForSelector:aSelector];
	} else {
		// method doesn't exits, throw an error!
		[NSException raise:@"CWRunnerException" format:@"filterTestMethodNames not found"];
	}

    NSArray *testMethods = nil;

    /* Test class methods */

    if (testClass != nil)
    {
		// filterTestMethodNames is just a C function, so we can call it directly.
		testMethods = filterTestMethodNames(runner, aSelector, UKTestMethodNamesFromClass(objc_getMetaClass(class_getName(testClass))) );
    }

    [self runTests: testMethods onInstance: NO ofClass: testClass];

    /* Test instance methods */

	// filterTestMethodNames is just a C function, so we can call it directly.
	testMethods = filterTestMethodNames(runner, aSelector, UKTestMethodNamesFromClass(testClass) );

    [self runTests: testMethods onInstance: YES ofClass: testClass];

    /* Test suite completed */
	NSLog(@"<COMPLETEDIN::>%d", (int)([[NSDate date] timeIntervalSinceDate: startSuiteDate] * 1000));
}
@end

#endif /* CWRunner_h */
