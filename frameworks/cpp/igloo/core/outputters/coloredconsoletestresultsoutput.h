//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_COLOREDCONSOLETESTRESULTSOUTPUT_H
#define IGLOO_COLOREDCONSOLETESTRESULTSOUTPUT_H

#include "consoleoutputcolors.h"

namespace igloo {

  class ColoredConsoleTestResultsOutput : public TestResultsOutput
  {
    public:
	  ColoredConsoleTestResultsOutput(std::ostream& outstream = std::cout) : TestResultsOutput(outstream) {}

      void PrintResult(const TestResults& results) const
      {
		for (TestResults::FailedTestsType::const_iterator it = results.FailedTests().begin(); it != results.FailedTests().end(); it++)
        {
          const FailedTestResult& result = *it;
          output << FAILED_OUTPUT_COLOR << result.GetContextName() << "::" << result.GetSpecName() << " failed:" << std::endl;

          if(result.HasLineNumber() && result.HasFilename())
          {
            output <<  result.Filename() << "(" << result.LineNumber() << "): Assertion failed." << std::endl << result.GetErrorMessage() << DEFAULT_OUTPUT_COLOR << std::endl;
          }
          else
          {
            output << result.GetErrorMessage() << DEFAULT_OUTPUT_COLOR << std::endl;
          }
        }

        output << ((results.NumberOfFailedTests() > 0) ? FAILED_OUTPUT_COLOR : PASSED_OUTPUT_COLOR) << "Test run complete. " << results.NumberOfTestsRun() << " tests run:\n" << results.NumberOfSucceededTests() << " succeeded\n" << results.NumberOfFailedTests() << " failed" << DEFAULT_OUTPUT_COLOR << std::endl;
      }
  };
}

#endif
