
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_VISUALSTUDIOOUTPUT_H
#define IGLOO_VISUALSTUDIOOUTPUT_H

namespace igloo {

  class VisualStudioResultsOutput : public TestResultsOutput
  {
    public:
      VisualStudioResultsOutput(std::ostream& outstream = std::cout) : TestResultsOutput(outstream) {}

      void PrintResult(const TestResults& results) const
      {
        TestResults::FailedTestsType::const_iterator it;

        for(it = results.FailedTests().begin(); it != results.FailedTests().end(); it++)
        {
          output << FormatOriginString(*it) << " : assertion failed error: " << (*it).GetContextName() << "::" << (*it).GetSpecName() << ":" << std::endl << (*it).GetErrorMessage() << std::endl;
        }

        output << "Test run complete. " << results.NumberOfTestsRun() << " tests run, " << results.NumberOfSucceededTests() << " succeeded, " << results.NumberOfFailedTests() << " failed." << std::endl;
      }

    private:

      std::string FormatOriginString(const FailedTestResult& result) const
      {
        if(result.HasLineNumber() && result.HasFilename())
        {
          std::ostringstream builder;
          builder << result.Filename() << "(" << result.LineNumber() << ")";
          return builder.str();
        }
        
        // Default to toolname if no location information is available
        return "Igloo";
      }
  };
}
#endif
