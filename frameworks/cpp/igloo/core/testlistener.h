
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_TESTLISTENER_H
#define IGLOO_TESTLISTENER_H

namespace igloo {

  struct ContextBase;

  class TestListener
  {
    public:
      virtual void TestRunStarting() = 0;
      virtual void TestRunEnded(const TestResults& results) = 0;
      virtual void ContextRunStarting(const ContextBase& context) = 0;
      virtual void ContextRunEnded(const ContextBase& context) = 0;
      virtual void SpecRunStarting(const ContextBase& context, const std::string& specName) = 0;
      virtual void SpecSucceeded(const ContextBase& context, const std::string& specName) = 0;
      virtual void SpecFailed(const ContextBase& context, const std::string& specName) = 0;
  };

  class NullTestListener : public TestListener
  {
    public:
      virtual void TestRunStarting() {}
      virtual void TestRunEnded(const TestResults&) {}
      virtual void ContextRunStarting(const ContextBase&) {}
      virtual void ContextRunEnded(const ContextBase&) {}
      virtual void SpecRunStarting(const ContextBase&, const std::string&) {}
      virtual void SpecSucceeded(const ContextBase&, const std::string&) {}
      virtual void SpecFailed(const ContextBase&, const std::string&) {}
  };
}

#endif
