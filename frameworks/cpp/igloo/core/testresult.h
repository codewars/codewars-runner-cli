
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_TESTRESULT_H
#define IGLOO_TESTRESULT_H

namespace igloo {

  class SucceededTestResult
  {
    public:
    SucceededTestResult(const std::string& contextName, const std::string& specName)
      : m_contextName(contextName), m_specName(specName)
    {
    }

    const std::string& GetContextName() const
    {
      return m_contextName;
    }

    const std::string& GetSpecName() const
    {
      return m_specName;
    }

    bool operator==(const SucceededTestResult& rhs) const
    {
      return m_contextName == rhs.m_contextName &&
      m_specName == rhs.m_specName;
    }
    friend std::ostream& operator<<(std::ostream& stream, const SucceededTestResult& res);

  private:
    std::string m_contextName;
    std::string m_specName;
  };

  inline std::ostream& operator<<(std::ostream& stream, const SucceededTestResult& res)
  {
    stream
      << "SucceededTestResult ( m_contextName=" << res.m_contextName
      << ", m_specName=" << res.m_specName
      << " )";

    return stream;
  }

  class FailedTestResult
  {
  public:
    FailedTestResult(const std::string& contextName, const std::string& specName, const std::string& errorMessage, const std::string& filename, unsigned int line)
      : m_contextName(contextName), m_specName(specName), m_errorMessage(errorMessage), m_filename(filename), m_line(line)
    {
    }

    FailedTestResult(const std::string& contextName, const std::string& specName, const std::string& errorMessage)
      : m_contextName(contextName), m_specName(specName), m_errorMessage(errorMessage), m_filename(""), m_line(0)
    {
    }

    const std::string& GetContextName() const
    {
      return m_contextName;
    }

    const std::string& GetSpecName() const
    {
      return m_specName;
    }

    const std::string& GetErrorMessage() const
    {
      return m_errorMessage;
    }

    bool HasLineNumber() const
    {
      return m_line != 0;
    }

    unsigned int LineNumber() const
    {
      return m_line;
    }
    
    bool HasFilename() const
    {
      return m_filename.length() > 0;
    }

    const std::string& Filename() const
    {
      return m_filename;
    }

    bool operator==(const FailedTestResult& rhs) const
    {
      return m_contextName == rhs.m_contextName &&
      m_specName == rhs.m_specName &&
      m_errorMessage == rhs.m_errorMessage &&
      m_filename == rhs.m_filename &&
      m_line == rhs.m_line;
    }
    
    friend std::ostream& operator<<(std::ostream& stream, const FailedTestResult& res);

  private:
    std::string m_contextName;
    std::string m_specName;
    std::string m_errorMessage;
    std::string m_filename;
    unsigned int m_line;
  };

  inline std::ostream& operator<<(std::ostream& stream, const FailedTestResult& res)
  {
    stream
      << "FailedTestResult ( m_contextName=" << res.m_contextName
      << ", m_specName=" << res.m_specName
      << ", m_errorMessage=" << res.m_errorMessage
      << ", m_filename=" << res.m_filename
      << ", m_line=" << res.m_line
      << " )";

    return stream;
  }

}

#endif /* IGLOO_TESTRESULT_H */

