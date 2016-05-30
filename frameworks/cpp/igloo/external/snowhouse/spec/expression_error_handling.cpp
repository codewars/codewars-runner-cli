
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>
using namespace igloo;

Context(ACollectionThatWeWantToVerify)
{
  void SetUp()
  {
    collection.push_back(1);
    collection.push_back(2);
    collection.push_back(3);
  }
  
  Spec(AnInvalidAllOperationShouldBeReportedProperly)
  {
    AssertTestFails(Assert::That(collection, Has().All()), "The expression after \"all\" operator does not yield any result");
  }
  
  Spec(AnInvalidAtLeastOperationShouldBeReportedProperly)
  {
    AssertTestFails(Assert::That(collection, Has().AtLeast(2)), "The expression after \"at least 2\" operator does not yield any result");
  }
  
  std::vector<int> collection;
};
