
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>

using namespace igloo;

Context(Strings)
{
  Spec(ShouldHandleStringContainsConstraint)
  {
    Assert::That("abcdef", Contains("bcde"));
  }

  Spec(StringConstraintShouldHandleMatchAtBeginningOfString)
  {
    Assert::That("abcdef", Contains("a"));
  }  

  Spec(ShouldDetectFailingContains)
  {
    AssertTestFails(Assert::That("abcdef", Contains("hello")), "contains hello");
  }

  Spec(ShouldHandleStringStartingWithConstraint)
  {
    Assert::That("abcdef", StartsWith("abc"));
  }

  Spec(ShouldHandleStringEndingWithConstraint)
  {
    Assert::That("abcdef", EndsWith("def"));
  }

  Spec(ShouldHandleOperatorsForStrings)
  {
    Assert::That("abcdef", StartsWith("ab") && EndsWith("ef"));
  }

  Spec(ShouldHandleStringsWithMultipleOperators)
  {
    Assert::That("abcdef", StartsWith("ab") && !EndsWith("qwqw"));
  }

  Spec(ShouldHandleOfLength)
  {
    Assert::That("12345", HasLength(5));
  }

  Spec(ShouldHandleWeirdLongExpressions)
  {
    Assert::That("12345", HasLength(5) && StartsWith("123") && !EndsWith("zyxxy"));
  }

  Spec(ShouldHandleStdStrings)
  {
    Assert::That("12345", Contains(std::string("23")));
  }

  Spec(ShouldHandleSimpleChar)
  {
    Assert::That("12345", StartsWith('1'));
  }
};
