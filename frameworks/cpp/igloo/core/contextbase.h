
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_CONTEXT_BASE_H
#define IGLOO_CONTEXT_BASE_H

namespace igloo {

  //
  // This is the base class for all contexts.
  // 
  struct ContextBase
  {
    virtual ~ContextBase() {}

    virtual void IglooFrameworkSetUp()
    {}

    virtual void IglooFrameworkTearDown()
    {}

    virtual void SetUp()
    {
    }

    virtual void TearDown()
    {
    }

    static void SetUpContext()
    {
    }

    static void TearDownContext()
    {
    }

    void SetName(const std::string& name)
    {
      m_name = name;
    }

    std::string Name() const
    {
      return m_name;
    }

    virtual void SetAttribute(const std::string& name, const char* value) const = 0;
    virtual const std::string& GetAttribute(const std::string& name) const = 0;

    private:
    std::string m_name;
  };

}
#endif
