
//          Copyright Bertrand Cachet 2012
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_XUNITOUTPUT_H
#define IGLOO_XUNITOUTPUT_H

#include <typeinfo>

#include <igloo/core/outputters/xmlwriter.h>

namespace igloo {
  class XUnitResultsOutput : public TestResultsOutput
  {
    public:

      XUnitResultsOutput(std::ostream& outstream = std::cout) 
		  : TestResultsOutput(outstream)
	  {
	  }

      void PrintResult(const TestResults& results) const
      {
        XmlWriter xw(this->output);
        XmlElement testsuite("testsuite", xw);

        testsuite.attr("name", "Igloo");
        testsuite.attr("tests", results.NumberOfTestsRun());
        testsuite.attr("errors", 0);
        testsuite.attr("failures", results.NumberOfFailedTests());
        for (TestResults::FailedTestsType::const_iterator it = results.FailedTests().begin();
                                                          it != results.FailedTests().end();
                                                          it++)
        {
          const FailedTestResult& result = *it;
          XmlElement testcase("testcase", xw);
          testcase.attr("classname", result.GetContextName());
          testcase.attr("name", result.GetSpecName());
          testcase.attr("time", 0);
            XmlElement failure("failure", xw);
            std::ostringstream builder;
            if(result.HasLineNumber() && result.HasFilename())
            {
              builder 	<< result.Filename() << "(" 
				  		<< result.LineNumber() << "): "
						<<	"assertion failed error: ";
            }

            builder << result.GetErrorMessage();
             failure.attr("message", builder.str());
        }

        for (TestResults::SucceededTestsType::const_iterator it = results.SucceededTests().begin(); 
                                                          	it != results.SucceededTests().end(); 
                                                          it++)
        {
           const SucceededTestResult& result = *it;
          XmlElement testcase("testcase", xw);
          testcase.attr("classname", result.GetContextName());
          testcase.attr("name", result.GetSpecName());
          testcase.attr("time", 0);
        }
      }
  };
}
#endif
