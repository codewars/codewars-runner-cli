
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>
#include <stdexcept>

using namespace igloo;

class ClassWithExceptions
{
public:
  int LogicError()
  {
    throw std::logic_error("not logical!");
  }
  
  double RangeError()
  {
    throw std::range_error("range error!");
  }
  
  void NoError()
  {
  }
};

Context(MethodsWithExceptions)
{
  ClassWithExceptions objectUnderTest;  
  
  
  Spec(CanDetectExceptions)
  { 
    AssertThrows(std::exception, objectUnderTest.LogicError());
  }
  
  Spec(CanAssertOnLastException)
  {
    AssertThrows(std::logic_error, objectUnderTest.LogicError());
    Assert::That(LastException<std::logic_error>().what(), Contains("not logical!"));
  }
  
  Spec(CanDetectWhenWrongExceptionIsThrown)
  {    
    AssertTestFails(AssertThrows(std::logic_error, objectUnderTest.RangeError()), "Wrong exception");
  }
  
  Spec(CanPrintExpectedExceptionTypeWhenWrongExceptionIsThrown)
  {
    AssertTestFails(AssertThrows(std::logic_error, objectUnderTest.RangeError()), "Expected std::logic_error");
  }

  Spec(CanHaveSeveralExceptionAssertionsInSameSpec)
  {
    AssertThrows(std::logic_error, objectUnderTest.LogicError());
    Assert::That(LastException<std::logic_error>().what(), Contains("not logical!"));

    AssertThrows(std::range_error, objectUnderTest.RangeError());
    Assert::That(LastException<std::range_error>().what(), Contains("range error!"));
  }
  
  Spec(CanHaveSeveralExceptionAssertionForTheSameExceptionInSameSpec)
  {
    AssertThrows(std::logic_error, objectUnderTest.LogicError());
    Assert::That(LastException<std::logic_error>().what(), Contains("not logical!"));

    AssertThrows(std::logic_error, objectUnderTest.LogicError());
    Assert::That(LastException<std::logic_error>().what(), Contains("not logical!"));
  }  
  
  Spec(CanDetectWhenNoExceptionIsThrown)
  {
    AssertTestFails(AssertThrows(std::logic_error, objectUnderTest.NoError()), "No exception");
  }

  Spec(CanPrintExpectedExceptionWhenNoExceptionIsThrown)
  {
    AssertTestFails(AssertThrows(std::logic_error, objectUnderTest.NoError()), "Expected std::logic_error");
  } 

  Spec(ExceptionsAreDestoryedWhenWeExitScope)
  {
    {
      AssertThrows(std::logic_error, objectUnderTest.LogicError());
    }
    AssertThrows(AssertionException, LastException<std::logic_error>());
    Assert::That(LastException<AssertionException>().GetMessage(), Contains("No exception was stored"));
  }  
};



