
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_CONTEXT_H
#define IGLOO_CONTEXT_H

#include <igloo/core/testresult.h>
#include <igloo/core/context.h>

namespace igloo {

  //
  // This class enables us to store attributes with contexts.
  // Attributes are named string values that can be used for
  // filtering and reporting etc.
  //
  // A testlistener can retrieve the attributes for a context
  // during a test run.
  // 
  template <typename ContextType>
    struct ContextAttributeStorage
    {
      static void Set(const std::string name, std::string value)
      {
        attributeContainer()[name] = value;
      }

      static const std::string& Get(const std::string& name)
      {
        return attributeContainer()[name];
      }

      private:

      static std::map<std::string, std::string>& attributeContainer()
      {
        static std::map<std::string, std::string> attributeContainer;
        return attributeContainer;
      }
    };


  //
  // This class is a base class to ContextProvider and adds
  // the possibility to add attributes to a context.
  //
  template <typename ContextType>
    struct ContextWithAttribute : ContextBase 
  {
    void SetAttribute(const std::string& name, const char* value) const
    {
      ContextAttributeStorage<ContextType>::Set(name, value);
    }

    const std::string& GetAttribute(const std::string& name) const
    {
      return ContextAttributeStorage<ContextType>::Get(name);
    }
  };
}

#endif
