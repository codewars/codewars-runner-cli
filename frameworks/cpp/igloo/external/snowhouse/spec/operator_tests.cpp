
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>

using namespace igloo;

Context(OperatorTests)
{
  Spec(ShouldHandleAndOperatorExpressionTemplates)
  {
    Assert::That(5, IsLessThan(6) && IsGreaterThan(4));
  } 

  Spec(ShouldHandleAndOperator)
  {
    Assert::That(5, Is().LessThan(6).And().GreaterThan(4));
  }

  Spec(ShouldHandleAndOperatorFailExpressionTemplates)
  {
    AssertTestFails(Assert::That(5, IsLessThan(7) && IsGreaterThan(5)), "less than 7 and greater than 5");
  } 

  Spec(ShouldHandleAndOperatorFail)
  {
    AssertTestFails(Assert::That(5, Is().LessThan(7).And().GreaterThan(5)), "less than 7 and greater than 5");
  }

  Spec(ShouldHandleOrOperator)
  {
    Assert::That(12, Is().LessThan(7).Or().GreaterThan(5));
  } 

  Spec(ShouldHandleOrOperatorExpressionTemplates)
  {
    Assert::That(12, IsLessThan(7) || IsGreaterThan(5));
  }

  Spec(ShouldHandleOrOperatorFails)
  {
    AssertTestFails(Assert::That(67, Is().LessThan(12).Or().GreaterThan(99)), "less than 12 or greater than 99");
  }
   
  Spec(ShouldHandleOrOperatorFailsExpressionTemplates)
  {
    AssertTestFails(Assert::That(67, IsLessThan(12) || IsGreaterThan(99)), "less than 12 or greater than 99");
  }
  
  Spec(ShouldHandleNotOperators)
  {
    Assert::That(5, Is().Not().EqualTo(4));
  }

  Spec(ShouldHandleNotOperatorsExpressionTemplates)
  {
    Assert::That(5, !Equals(4));
  }

  Spec(ShouldHandleNotOperatorsFails)
  {
    AssertTestFails(Assert::That(12, Is().Not().EqualTo(12)), "not equal to 12");
  } 

  Spec(ShouldHandleNotOperatorsFailsExpressionTemplates)
  {
    AssertTestFails(Assert::That(12, !Equals(12)), "not equal to 12");
  }

  Spec(ShouldHandleNotOperatorsForStrings)
  {
    Assert::That("joakim", Is().Not().EqualTo("harry"));
  } 

  Spec(ShouldHandleNotOperatorsForStringsExpressionTemplates)
  {
    Assert::That("joakim", !Equals("harry"));
  }

  Spec(ShouldHandleBothLeftAndRightAssociativeOperators)
  {
    Assert::That(5, Is().GreaterThan(4).And().Not().LessThan(3));
  } 

  Spec(ShouldHandleBothLeftAndRightAssociativeOperatorsExpressionTemplates)
  {
    Assert::That(5, IsGreaterThan(4)&& !IsLessThan(3));
  }
   
  Spec(MalformedExpressionYieldsError)
  {
    AssertTestFails(Assert::That(4, Is().Not()), "The expression contains a not operator without any operand");
  }

  Spec(EqualsWithDeltaOperator_should_fail_for_actual_larger_than_delta)
  {
    AssertTestFails(Assert::That(3.9, EqualsWithDelta(3, 0.5)), "Expected: equal to 3 (+/- 0.5)");
  }

  Spec(EqualsWithDeltaOperator_should_fail_for_actual_less_than_delta)
  {
    AssertTestFails(Assert::That(2.49, EqualsWithDelta(3, 0.5)), "Expected: equal to 3 (+/- 0.5)");
  }

  Spec(EqualsWithDeltaOperator_should_succeed)
  {
    Assert::That(2, EqualsWithDelta(1.9, 0.1));
  }

  Spec(Fluent_equals_with_delta_should_fail_for_actual_larger_than_delta)
  {
    AssertTestFails(Assert::That(3.9, Is().EqualToWithDelta(3, 0.5)), "Expected: equal to 3 (+/- 0.5)");
  }

  Spec(Fluent_EqualsWithDeltaOperator_should_fail_for_actual_less_than_delta)
  {
    AssertTestFails(Assert::That(2.49, Is().EqualToWithDelta(3, 0.5)), "Expected: equal to 3 (+/- 0.5)");
  }

  Spec(Fluent_EqualsWithDeltaOperator_should_succeed)
  {
    Assert::That(2, Is().EqualToWithDelta(1.9, 0.1));
  }

};
