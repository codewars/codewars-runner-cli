
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_TESTRUNNER_H
#define IGLOO_TESTRUNNER_H

#include <igloo/core/testresult.h>
#include <igloo/core/testresults.h>
#include <igloo/core/testlisteners/minimalprogresstestlistener.h>
#include <igloo/external/choices/choices.h>

namespace igloo {

  namespace c = choices;

  class TestRunner 
  {
    public:
      typedef std::list<BaseContextRunner*> ContextRunners;

      static int RunAllTests(int argc = 0, const char *argv[] = 0)
      {
        choices::options opt = choices::parse_cmd(argc, argv);

        if(c::has_option("version", opt))
        {
          std::cout << IGLOO_VERSION << std::endl;
          return 0;
        }

        if(c::has_option("help", opt))
        {
          std::cout << "Usage: <igloo-executable> [--version] [--output=color|vs|xunit]" << std::endl;
          std::cout << "Options:" << std::endl;
          std::cout << "  --version:\tPrint version of igloo and exit." << std::endl;
          std::cout << "  --output:\tFormat output of test results." << std::endl;
          std::cout << "\t\t--output=color:\tColored output" << std::endl;
          std::cout << "\t\t--output=vs:\tVisual studio friendly output." << std::endl;
          std::cout << "\t\t--output=xunit:\tXUnit formatted output." << std::endl;
          std::cout << "\t\t--output=default:\tDefault output format." << std::endl;
          return 0;
        }

        std::auto_ptr<TestResultsOutput> output;
        if(c::has_option("output", opt))
        {
          std::string val = c::option_value("output", opt);
          if(val == "vs")
          {
            output = std::auto_ptr<TestResultsOutput>(new VisualStudioResultsOutput());
          }
          else if(val == "color")
          {
            output = std::auto_ptr<TestResultsOutput>(new ColoredConsoleTestResultsOutput());
          }
          else if(val == "xunit")
          {
            output = std::auto_ptr<TestResultsOutput>(new XUnitResultsOutput());
          }
          else if(val == "default")
          {
            output = std::auto_ptr<TestResultsOutput>(new DefaultTestResultsOutput());
          }
          else
          {
            std::cerr << "Unknown output: " << c::option_value("output", opt) << std::endl;
            return 1;
          }
        }
        else
        {
          output = std::auto_ptr<TestResultsOutput>(new DefaultTestResultsOutput());
        }


        TestRunner runner(*(output.get()));

        MinimalProgressTestListener progressOutput;
        runner.AddListener(&progressOutput);

        return runner.Run();
      }


      TestRunner(const TestResultsOutput& output) 
        : output_(output)
      {}

      static bool is_only(const BaseContextRunner* runner)
      {
        return runner->IsContextMarkedAsOnly();
      }

      int Run(const ContextRunners& runners)
      {
        TestResults results;

        listenerAggregator_.TestRunStarting();

        bool only_has_been_found = std::find_if(runners.begin(), runners.end(), is_only) != runners.end();

        for (ContextRunners::const_iterator it = runners.begin(); it != runners.end(); it++)
        {
          BaseContextRunner* contextRunner = *it;
          if(!only_has_been_found || contextRunner->IsContextMarkedAsOnly()) {
            if(!contextRunner->IsMarkedAsSkip()) {
              contextRunner->Run(results, listenerAggregator_);
            }
          }
        }

        listenerAggregator_.TestRunEnded(results);

        output_.PrintResult(results);
        return results.NumberOfFailedTests();
      }

      int Run()
      {
        int numberOfFailedTests = Run(RegisteredRunners());
        CleanUpRunners();
        return numberOfFailedTests;
      }


      template <typename ContextRunnerType>
        static void RegisterContext(const std::string& name, const char* fileName, int lineNumber)
        {
          if(!ContextIsRegistered(name, fileName, lineNumber))
          {
            ContextRunnerType* contextRunner = 0;

            try
            {
              // Must add runner first...
              contextRunner = new ContextRunnerType(name, fileName, lineNumber);
              TestRunner::RegisteredRunners().push_back(contextRunner);

              // ... and then instantiate context, because context ctor calls this method again,
              // possibly for the same context, depending on inheritance chain.
              contextRunner->InstantiateContext();
            }
            catch (...)
            {
              delete contextRunner;
              throw;
            }
          }
        }

      void AddListener(TestListener* listener)
      {
        listenerAggregator_.AddListener(listener);
      }

    private:
      static void CleanUpRunners()
      {
        while(!RegisteredRunners().empty())
        {
          delete RegisteredRunners().front();
          RegisteredRunners().pop_front();
        }
      }

      static bool ContextIsRegistered(const std::string& name, const char* fileName, int lineNumber)
      {
        for (ContextRunners::const_iterator it = RegisteredRunners().begin(); it != RegisteredRunners().end(); ++it)
        {
          if((*it)->ContextName() == name &&
             (*it)->FileName() == fileName &&
             (*it)->LineNumber() == lineNumber)
          {
            return true;
          }
        }

        return false;
      }

      static TestRunner::ContextRunners& RegisteredRunners()
      {
        static TestRunner::ContextRunners contextRunners;
        return contextRunners;
      }

    private:
      const TestResultsOutput& output_;
      TestListenerAggregator listenerAggregator_;
  };
}

#endif // IGLOO_TESTRUNNER_H
