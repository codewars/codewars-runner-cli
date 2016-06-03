
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef TESTRESULTFACTORY_H
#define TESTRESULTFACTORY_H

namespace igloo {

  class TestResultFactory
  {
    public:
      TestResultFactory(const std::string& contextName, const std::string& specName)
        : m_contextName(contextName), m_specName(specName)
      {}

      FailedTestResult CreateFromException(const snowhouse::AssertionException& exception) const
      {
        return FailedTestResult(m_contextName, m_specName, exception.GetMessage(), exception.GetFilename(), exception.GetLineNumber());
      }

      SucceededTestResult CreateSuccessful() const
      {
        return SucceededTestResult(m_contextName, m_specName);
      }

    private:
      std::string m_contextName;
      std::string m_specName;
  };
}
#endif
