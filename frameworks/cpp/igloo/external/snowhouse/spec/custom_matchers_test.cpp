
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>

using namespace igloo;

Context(CustomMatcherWithoutStreamOperatorOverloaded)
{
  struct IsEvenNumber
  {
    bool Matches(const int actual) const
    {
      return (actual % 2) == 0; 
    }
  };

  Spec(CanHandleCustomMatcher)
  {
    Assert::That(2, Fulfills(IsEvenNumber()));
  }

  Spec(CustomMatcherWithFluent)
  {
    Assert::That(2, Is().Fulfilling(IsEvenNumber()));
  }

  Spec(OutputsCorrectMessageWhenFails)
  {
    AssertTestFails(Assert::That(3, Fulfills(IsEvenNumber())), "Expected: [unsupported type]\nActual: 3");
  }
};

struct IsEvenNumber
{
  bool Matches(const int actual) const
  {
    return (actual % 2) == 0; 
  }

  friend std::ostream& operator<<(std::ostream& stm, const IsEvenNumber& );
};

std::ostream& operator<<(std::ostream& stm, const IsEvenNumber& )
{
  stm << "An even number";
  return stm;
}

Context(CustomMatcherWithStreamOperator)
{
  Spec(ErrorMessageUsesCustomStreamOperatorIfAvailable)
  {
    AssertTestFails(Assert::That(3, Fulfills(IsEvenNumber())), "Expected: An even number\nActual: 3");
  }
};
