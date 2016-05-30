
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>

using namespace igloo;

Context(MapTests)
{
  std::map<std::string, int> ages;
  
  void SetUp()
  {
    ages["joakim"] = 38;
    ages["maria"] = 36;
    ages["hanna"] = 6;
    ages["moa"] = 4;
  }
  
  Spec(ContainingShouldDetermineIfKeyExists)
  {
    Assert::That(ages, Is().Containing("joakim"));
  }
  
  Spec(ShouldGiveAProperMessageWhenContainingFails)
  {
    AssertTestFails(Assert::That(ages, Is().Not().Containing("hanna")), "Expected: not contains hanna"); 
  }
};

Context(MapTests_ExpressionTemplates)
{
  std::map<std::string, int> ages;
  
  void SetUp()
  {
    ages["joakim"] = 38;
    ages["maria"] = 36;
    ages["hanna"] = 6;
    ages["moa"] = 4;
  }
  
  Spec(ContainingShouldDetermineIfKeyExists)
  {
    Assert::That(ages, Contains("joakim"));
  }
  
  Spec(ShouldGiveAProperMessageWhenContainingFails)
  {
    AssertTestFails(Assert::That(ages, !Contains("hanna")), "Expected: not contains hanna"); 
  }
};
