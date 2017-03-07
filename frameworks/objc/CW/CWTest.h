//
//  CWTest.h
//  Test
//
//  Created by Ido on 1/9/17.
//  Copyright © 2017 Codewars. All rights reserved.
//

#ifndef CWTest_h
#define CWTest_h

@interface CWTest: NSObject

+ (void) format:(NSString*)text withTag:(NSString*)tag withHandler:(void(^)(void))handler;
+ (void) describe:(NSString*)text withHandler:(void(^)(void))handler;
+ (void) it:(NSString*)text withHandler:(void(^)(void))handler;
+ (NSDate*) start;
+ (void) end:(NSDate*)date;
+ (void) pass:(int)expression;
+ (void) equal:(id)a withB:(id)b;
+ (void) notEqual:(id)a withB:(id)b;

@end

@implementation CWTest

+ (void) format:(NSString*)text withTag:(NSString*)tag withHandler:(void(^)(void))handler
{
    // Replace all the new line (\n) with <:LF:>
    text = [text stringByReplacingOccurrencesOfString:@"\n" withString:@"<:LF:>"];

    NSLog(@"%@", [NSString stringWithFormat:@"%@%@", tag, text]);
    handler();
}

+ (void) describe:(NSString*)text withHandler:(void(^)(void))handler
{
    [CWTest format:text withTag:@"<DESCRIBE::>" withHandler:handler];
}

+ (void) it:(NSString*)text withHandler:(void(^)(void))handler
{
    [CWTest format:text withTag:@"<IT::>" withHandler:handler];
}

+ (NSDate*) start
{
    return [NSDate date];
}

+ (void) end:(NSDate*)date
{
    int timePassed_ms = [date timeIntervalSinceNow] * -1000.0;
    NSLog(@"%@", [NSString stringWithFormat:@"<COMPLETEDIN::>%d", timePassed_ms]);
}

+ (void) pass:(int)expression
{
    if (expression)
    {
        NSLog(@"<PASSED::>Test Passed\n");
    }
    else
    {
        NSLog(@"<PASSED::>Test Failed\n");
    }
}

+ (void) equal:(id)a withB:(id)b
{
    if ([a isEqual: b])
    {
        NSLog(@"<PASSED::>Test Passed\n");
    }
    else
    {
        NSString* expectedClass = NSStringFromClass([a class]);
        NSString* actualClass = NSStringFromClass([b class]);
        NSLog(@"%@", [NSString stringWithFormat:@"<FAILED::>Expected \"%@\" (%@) but instead got \"%@\" (%@)\n", a, expectedClass, b, actualClass]);
    }
}

+ (void) notEqual:(id)a withB:(id)b
{
    if ([a isEqual: b])
    {
        NSString* aClass = NSStringFromClass([a class]);
        NSLog(@"%@", [NSString stringWithFormat:@"<FAILED::>Value is not supposed to equal \"%@\" (%@)\n", a, aClass]);
    }
    else
    {
        NSLog(@"<PASSED::>Test Passed\n");
    }
}

@end

#define describe(text, handler) \
{ \
    NSDate* date = [CWTest start]; \
    [CWTest describe:text withHandler:handler]; \
    [CWTest end:date]; \
}

#define it(text, handler) \
{ \
    NSDate* date = [CWTest start]; \
    [CWTest it:text withHandler:handler]; \
    [CWTest end:date]; \
}

#define pass(expression) \
{ \
    [CWTest pass:(int)(expression)]; \
}

#define equal(a, b) \
{ \
    [CWTest equal:a withB:b]; \
}

#define notEqual(a, b) \
{ \
    [CWTest notEqual:a withB:b]; \
}

#endif /* CWTest_h */