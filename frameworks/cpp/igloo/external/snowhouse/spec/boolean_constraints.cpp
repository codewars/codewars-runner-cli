
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>
using namespace igloo;

Context(BooleanOperators)
{
  Spec(ShouldHandleIsFalseOperator)
  {
    Assert::That(false, IsFalse());
  }

  Spec(ShouldHandleWhenIsFalseFails)
  {
    AssertTestFails(Assert::That(true, IsFalse()), "Expected: false");
  }

  Spec(ShouldHandleIsTrueOperator)
  {
    Assert::That(true, IsTrue());
  }

  Spec(ShouldHandleWhenIsTrueFails)
  {
    AssertTestFails(Assert::That(false, IsTrue()), "Expected: true");
  }

  Spec(ShouldHandleFluentIsTrue)
  {
    Assert::That(true, Is().True());
    AssertTestFails(Assert::That(false, Is().True()), "Expected: true");
  }

  Spec(ShouldHandleFluentIsFalse)
  {
    Assert::That(false, Is().False());
    AssertTestFails(Assert::That(true, Is().False()), "Expected: false");
  }

  Spec(ShouldTreatAssertWithoutConstraintAsBooleanConstrains)
  {
    Assert::That(true);
  }
};

