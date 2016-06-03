
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_CONTEXTRUNNER_H
#define IGLOO_CONTEXTRUNNER_H

namespace igloo {

  
  struct BaseContextRunner 
  {
    BaseContextRunner(const std::string& contextName, const char* fileName, int lineNumber) 
      : contextName_(contextName), fileName_(fileName), lineNumber_(lineNumber) {}
    virtual ~BaseContextRunner() {}
    void Run(TestResults& results, TestListener& testListener) const
    {
      RunContext(results, testListener);
    }

    virtual bool IsContextMarkedAsOnly() const = 0;

    virtual bool IsMarkedAsSkip() const = 0;

    const std::string& ContextName() const
    {
      return contextName_;
    }

    const std::string& FileName() const
    {
      return fileName_;
    }

    int LineNumber() const
    {
      return lineNumber_;
    }

    protected:
    virtual void RunContext(TestResults& results, TestListener& testListener) const = 0;

    private:
      std::string contextName_;
      std::string fileName_;
      int lineNumber_;
  };
  
  template <typename ContextRegistryType, typename ContextType>
  struct ContextSelector
  {
    typedef ContextRegistryType ContextToExecute;
    typedef ContextType ContextToCreate;
  };
  
  template <typename ContextType>
  struct ContextSelector<void, ContextType>
  {
    typedef ContextType ContextToExecute;
    typedef ContextType ContextToCreate;
  };

  template <typename ContextRegistryType, typename ContextType>
  class ContextRunner : public BaseContextRunner
  {
  public:   
    typedef typename ContextSelector<ContextRegistryType, ContextType>::ContextToExecute CTE;
    typedef typename ContextSelector<ContextRegistryType, ContextType>::ContextToCreate CTC;

    ContextRunner(const std::string& contextName, const char* fileName, int lineNumber) 
      : BaseContextRunner(contextName, fileName, lineNumber) {}

    void InstantiateContext() const
    {
      CTC ctc;
    }

    virtual bool IsContextMarkedAsOnly() const
    {
      return ContextType::IsContextMarkedAsOnly();
    }
    
    virtual bool IsMarkedAsSkip() const
    {
      return ContextType::IsMarkedAsSkip();
    }

    void RunContext(TestResults& results, TestListener& testListener) const
    {
      typedef ContextRegistry<CTE> CR;
      CR::template Run<CTC>(ContextName(), results, testListener);
    }
  };
}

#endif
