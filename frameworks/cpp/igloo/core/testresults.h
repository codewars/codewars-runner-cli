
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_TESTRESULTS_H
#define IGLOO_TESTRESULTS_H

namespace igloo {

  class TestResults 
  {
    public:

      int NumberOfTestsRun() const
      {
        return NumberOfSucceededTests() + NumberOfFailedTests();
      }

      int NumberOfSucceededTests() const
      {
        return succeededTests_.size();
      }

      int NumberOfFailedTests() const
      {
        return failedTests_.size();
      }

      void AddResult(const SucceededTestResult result)
      {
        succeededTests_.push_back(result);
      }

      void AddResult(const FailedTestResult result)
      {
        failedTests_.push_back(result);
      }

      typedef std::list<FailedTestResult> FailedTestsType;
      typedef std::list<SucceededTestResult> SucceededTestsType;

      const FailedTestsType& FailedTests() const
      {
        return failedTests_;
      }

      const SucceededTestsType& SucceededTests() const
      {
        return succeededTests_;
      }

      friend std::ostream& operator<<(std::ostream& stm, const TestResults& results);

    private:
      std::list<FailedTestResult> failedTests_;
      std::list<SucceededTestResult> succeededTests_;
  };


  inline std::ostream& operator<<(std::ostream& stm, const TestResults& results)
  {
    stm << "[ ";
    TestResults::FailedTestsType::const_iterator it;
    for(it = results.FailedTests().begin(); it != results.FailedTests().end(); it++)
    {
      stm << "< " << *it << " >";
    }

    TestResults::SucceededTestsType::const_iterator it2;
    for(it2 = results.SucceededTests().begin(); it2 != results.SucceededTests().end(); it2++)
    {
      stm << "< " << *it2 << " >";
    }

    stm << " ]";

    return stm;
  }
}

#endif
