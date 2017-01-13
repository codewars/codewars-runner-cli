//
//  CWTest.h
//  Test
//
//  Created by Ido on 1/9/17.
//  Copyright © 2017 Codewars. All rights reserved.
//

#ifndef CWTest_h
#define CWTest_h

#import <Foundation/Foundation.h>

static NSDate *CWTest_date = nil;


void CWTest_format(NSString* text, NSString* tag, void (* handler)(void))
{
    // Replace all the new line (\n) with <:LF:>
    text = [text stringByReplacingOccurrencesOfString:@"\n" withString:@"<:LF:>"];

    NSLog(@"%@", [NSString stringWithFormat:@"%@ %@", tag, text]);
    handler();
}

void CWTest_describe(NSString* text, void (* handler)(void))
{
    CWTest_format(text, @"<DESCRIBE::>", handler);
}

void CWTest_it(NSString* text, void (* handler)(void))
{
    CWTest_format(text, @"<IT::> It", handler);
}

void CWTest_start()
{
    CWTest_date = [NSDate date];
}

void CWTest_end()
{
    int timePassed_ms = [CWTest_date timeIntervalSinceNow] * -1000.0;
    NSLog(@"%@", [NSString stringWithFormat:@"<COMPLETEDIN::>%d", timePassed_ms]);
}

@interface CWTest: NSObject

+ (void) format:(NSString*)text withTag:(NSString*)tag withHandler:(void(^)(void))handler;
+ (void) describe:(NSString*)text withHandler:(void(^)(void))handler;
+ (void) it:(NSString*)text withHandler:(void(^)(void))handler;
+ (void) start;
+ (void) end;
+ (void) pass:(int)expression;
+ (void) equal:(id)a withB:(id)b;

@end

@implementation CWTest

+ (void) format:(NSString*)text withTag:(NSString*)tag withHandler:(void(^)(void))handler
{
    // Replace all the new line (\n) with <:LF:>
    text = [text stringByReplacingOccurrencesOfString:@"\n" withString:@"<:LF:>"];

    NSLog(@"%@", [NSString stringWithFormat:@"%@ %@", tag, text]);
    handler();
}

+ (void) describe:(NSString*)text withHandler:(void(^)(void))handler
{
    [CWTest format:text withTag:@"<DESCRIBE::>" withHandler:handler];
}

+ (void) it:(NSString*)text withHandler:(void(^)(void))handler
{
    [CWTest format:text withTag:@"<IT::> It" withHandler:handler];
}

+ (void) start
{
    CWTest_date = [NSDate date];
}

+ (void) end
{
    int timePassed_ms = [CWTest_date timeIntervalSinceNow] * -1000.0;
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
        NSLog(@"%@", [NSString stringWithFormat:@"<FAILED::>Expected \"%@\" but instead got \"%@\"\n", a, b]);
    }
}

@end

#define describe(text, handler) \
{ \
    [CWTest describe:text withHandler:handler]; \
}

#define it(text, handler) \
{ \
    [CWTest start]; \
    [CWTest it:text withHandler:handler]; \
    [CWTest end]; \
}

#define pass(expression) \
{ \
    [CWTest pass:(int)(expression)]; \
}

#define equal(a, b) \
{ \
    [CWTest equal:a withB:b]; \
}

#endif /* CWTest_h */
