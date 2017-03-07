/**
 * CWRunner.h
 * A test runner wrapper for UnitKit/Codewars
 *
 * Created by  Ivano Picco <ivano.picco@pianobit.com>
 *
 **/

#import <Foundation/Foundation.h>
#import <UnitKit/UnitKit.h>

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
 [runner release];
 [super dealloc];
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
    NSLog(@"<COMPLETEDIN::>%d", (int)([[NSDate date] timeIntervalSinceDate: startTestDate] * 1000));
}

- (void)reportWarning: (NSString *)message
{
	_testsFailed++; 
	_exceptionsReported++;
    message = [message stringByReplacingOccurrencesOfString:@"\n" withString:@"<:LF:>"];
    NSLog(@"<FAILED::>%s\n", message.UTF8String);
    NSLog(@"<COMPLETEDIN::>%d", (int)([[NSDate date] timeIntervalSinceDate: startTestDate] * 1000));
}

- (void)runTests: (NSArray *)testMethods onInstance: (BOOL)instance ofClass: (Class)testClass
{
	Class cls = object_getClass(runner);
	Ivar _ivarReleasing = class_getInstanceVariable(cls, "_releasing");
	id _releasing;

    for (NSString *testMethodName in testMethods)
    {
    	NSLog(@"<IT::>%s",testMethodName.UTF8String);
    	startTestDate = [NSDate date];
    	_releasing= object_getIvar(runner,_ivarReleasing);
        @try
        {
            @autoreleasepool
            {

				#pragma clang diagnostic push
				#pragma clang diagnostic ignored "-Wobjc-method-access"
                [runner runTestNamed: testMethodName
                        onInstance: instance
                           ofClass: testClass];
				#pragma clang diagnostic pop
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
    }
}

- (void)runSuite: (Class)testClass
{
	    NSLog(@"<DESCRIBE::>%s", class_getName(testClass));
	    NSDate *startSuiteDate = [NSDate date];

		//[runner runTestsInBundle: [NSBundle mainBundle]];
		//[runner runTestsWithClassNames: @[@"TestSuite"] principalClass:[testSuite class]];
		//[runner runTestsInClass: [testSuite class]];

	    NSArray *testMethods = nil;

	    /* Test class methods */

	    if (testClass != nil)
	    {
	    	#pragma clang diagnostic push
			#pragma clang diagnostic ignored "-Wobjc-method-access"
	        testMethods = [runner filterTestMethodNames: UKTestMethodNamesFromClass(objc_getMetaClass(class_getName(testClass)))];
	        #pragma clang diagnostic pop
	    }
	    [self runTests: testMethods onInstance: NO ofClass: testClass];

	    /* Test instance methods */

		#pragma clang diagnostic push
		#pragma clang diagnostic ignored "-Wobjc-method-access"
	    testMethods = [runner filterTestMethodNames: UKTestMethodNamesFromClass(testClass)];
	    #pragma clang diagnostic pop
	    [self runTests: testMethods onInstance: YES ofClass: testClass];

	    /* Test suite completed */
		NSLog(@"<COMPLETEDIN::>%d", (int)([[NSDate date] timeIntervalSinceDate: startSuiteDate] * 1000));
}
@end

#endif /* CWRunner_h */