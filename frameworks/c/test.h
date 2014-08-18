#ifndef _CODEWARS_TEST_H_
#define _CODEWARS_TEST_H_

int __CODEWARS__stdout__(const char * first, ...);
char * __CODEWARS__default_string__(char * default_string, ...);
int __CODEWARS__fail__(const char * message);
char * format(const char * format_string, ...);

#define DESCRIBE(x)                                             \
  __CODEWARS__stdout__("<DESCRIBE::>", (x), "<:LF:>\n", 0);

#define IT(x)                                           \
  __CODEWARS__stdout__("<IT::>", (x), "<:LF:>\n", 0);

#define EXPECT(fact, message...)                                        \
  if((fact)) __CODEWARS__stdout__("<PASSED::>Test Passed\n",0);         \
  else __CODEWARS__fail__(__CODEWARS__default_string__("Value is not what was expected", ##message, 0));

#define EXPECT_ERROR(fact, message...)                                  \
  if(!(fact)) __CODEWARS__stdout__("<PASSED::>Test Passed\n",0);        \
  else __CODEWARS__fail__(__CODEWARS__default_string__("Value is not what was expected", ##message, 0));

#define ASSERT_EQUALS(value1, value2, message...)                       \
  if((value1) == (value2)) __CODEWARS__stdout__("<PASSED::>Test Passed\n",0); \
  else __CODEWARS__fail__(__CODEWARS__default_string__("Values are not equal", ##message, 0));

#define ASSERT_NOT_EQUALS(value1, value2, message...)                   \
  if((value1) == (value2)) __CODEWARS__stdout__("<PASSED::>Test Passed\n",0); \
  else __CODEWARS__fail__(__CODEWARS__default_string__("Values should not equal", ##message, 0));

#endif _CODEWARS_TEST_H_
