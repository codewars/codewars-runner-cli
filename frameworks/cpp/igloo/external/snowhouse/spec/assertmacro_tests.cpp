
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>
using namespace igloo;

struct HasLineNumber
{
  bool Matches(const FailedTestResult& result) const
  {
    return result.HasLineNumber();
  }
};

std::ostream& operator<<(std::ostream& stm, const HasLineNumber& )
{
  stm << "FailedTestResult has line number information";
  return stm;
}

struct HasFileInformation
{
  bool Matches(const FailedTestResult& result) const
  {
    return result.HasFilename();
  }
};

std::ostream& operator<<(std::ostream& stm, const HasFileInformation& )
{
  stm << "FailedTestResult has filename information";
  return stm;
}

Context(A_context_with_failing_specs)
{
  struct FailingContext : public ContextProvider<FailingContext, ContextWithAttribute<void>, void, false, false >
  {
    Spec(Assert_without_file_and_line_info)
    {
      Assert::That(3, Equals(5));
    }

    Spec(Assert_with_file_and_line_info)
    {
      AssertThat(23, IsGreaterThan(99));
    }
  } failing_context;

  Spec(TestResult_does_not_contain_line_and_file_info_when_none_is_provided_in_assert)
  {
    TestResults results;
    ContextRegistry<FailingContext>::CallSpec(failing_context, "Assert_without_file_and_line_info", &FailingContext::Assert_without_file_and_line_info, results);

    Assert::That(results.FailedTests(), Has().Exactly(1).Not().Fulfilling(HasLineNumber()).And().Not().Fulfilling(HasFileInformation()));
  }

  Spec(TestResult_contains_line_and_file_info_when_provided_in_assert)
  {
    TestResults results;
    ContextRegistry<FailingContext>::CallSpec(failing_context, "Assert_with_file_and_line_info", &FailingContext::Assert_with_file_and_line_info, results);

    Assert::That(results.FailedTests(), Has().Exactly(1).Fulfilling(HasLineNumber()).And().Fulfilling(HasFileInformation()));
  }
};

