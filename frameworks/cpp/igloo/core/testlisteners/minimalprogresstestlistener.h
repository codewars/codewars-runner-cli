
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_MINIMALPROGRESSTESTLISTENER_H
#define IGLOO_MINIMALPROGRESSTESTLISTENER_H

namespace igloo {

  class MinimalProgressTestListener : public TestListener
  {
    public:
      virtual void TestRunStarting() {}
      virtual void TestRunEnded(const TestResults&) 
      {
        // std::cout << std::endl;
      }

      virtual void ContextRunStarting(const ContextBase& ) {}
      virtual void ContextRunEnded(const ContextBase& ) {}
      virtual void SpecRunStarting(const ContextBase& , const std::string& ) {}
      virtual void SpecSucceeded(const ContextBase& , const std::string& )
      {
        // std::cout << ".";
      }

      virtual void SpecFailed(const ContextBase& , const std::string&, const FailedTestResult& )
      {
        // std::cout << "F";
      }
  };

}
#endif
