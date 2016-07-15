
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_CONTEXTREGISTRY_H
#define IGLOO_CONTEXTREGISTRY_H

namespace igloo {

  //
  // This class stores information about all specs
  // registered for a context type.
  //
  // There are two template types in play here, one at 
  // the class level, and one for the "Run()" method.
  //
  // We separate between 'ContextToCall' and 'ContextToCreate'.
  // This is used for creating abstract test cases where we
  // have an abstract base class with test methods (ContextToCall), and
  // multiple concrete test classes that set up specific versions of 
  // test environments ('ContextToCreate').
  //
  template <typename ContextToCall>
  class ContextRegistry  
  {
    typedef void (ContextToCall::*SpecPtr)();


    //
    // Information about a spec.
    // Contains a pointer to the spec method, plus
    // information about whether the spec is marked
    // as "only" or "skip".
    //
    struct SpecInfo
    {
      SpecPtr spec_ptr;
      std::string name;
      bool skip;
      bool only;
    };

    typedef std::list<SpecInfo> Specs;

    public:
    //
    // Register a new spec (this is called by the registration
    // macros during a live run).
    //
    static void RegisterSpec(const std::string& name, void (ContextToCall::*spec)(), 
        bool skip = false, bool only = false)
    {
      SpecInfo spec_info;
      spec_info.spec_ptr = spec;
      spec_info.skip = skip;
      spec_info.name = name;
      spec_info.only = only;
      GetSpecs().push_back(spec_info);
    }

    static void ClearRegisteredSpecs()
    {
      GetSpecs().clear();
    }

    template <typename ContextToCreate>
      static void Run(const std::string& contextName, TestResults& results,
          TestListener& testListener)
      {    
        Specs specs;
        GetSpecsToRun(specs);
        CallSpecs<ContextToCreate>(specs, contextName, results, testListener);
      }


    template <typename ContextToCreate>
      static void CallSpecs(const Specs& specs, const std::string& contextName,
          TestResults& results, TestListener& testListener)
      {
        ContextToCreate::SetUpContext();

        ContextToCreate c;
        c.SetName(contextName);

        testListener.ContextRunStarting(c);

        typename Specs::const_iterator it;
        for (it = specs.begin(); it != specs.end(); it++)
        {
          SpecInfo spec_info = *it;
          const std::string& specName = spec_info.name;

          ContextToCreate context;
          context.SetName(contextName);

          testListener.SpecRunStarting(context, specName);

          if(!spec_info.skip)
          {
            if(CallSpec(context, specName, spec_info.spec_ptr, results))
            {
              testListener.SpecSucceeded(context, specName); 
            }
            else
            {
              for(auto& result:results.FailedTests()) {
                if(result.GetSpecName() == specName) {
                  testListener.SpecFailed(context, specName, result);
                }
              }
            }
          }
        }

        ContextToCreate::TearDownContext();

        testListener.ContextRunEnded(c);
      }

    static bool CallSpec(ContextToCall& context, const std::string& specName,
        SpecPtr spec, TestResults& results)
    {
      bool result = true;

      try
      {
        context.IglooFrameworkSetUp();
        (context.*spec)();
      }
      catch (const snowhouse::AssertionException& e)
      {
        results.AddResult(TestResultFactory(context.Name(), specName).CreateFromException(e));
        result = false;
      }
      catch (...)
      {
        results.AddResult(FailedTestResult(context.Name(), specName, "Caught unknown exception"));
        result = false;
      }

      try 
      {
        context.IglooFrameworkTearDown();
      }
      catch (const snowhouse::AssertionException& e) 
      {
        results.AddResult(TestResultFactory(context.Name(), specName).CreateFromException(e));
        result = false;
      }
      catch (...)
      {
        results.AddResult(FailedTestResult(context.Name(), specName, "Caught unknown exception"));
        result = false;
      }

      if(result)
      {
        results.AddResult(TestResultFactory(context.Name(), specName).CreateSuccessful());
      }

      return result;
    }

    static Specs& GetSpecs()
    {
      static Specs specs;
      return specs;
    }

    static bool is_spec_not_marked_as_only(const SpecInfo& spec)
    {
      return !is_spec_marked_as_only(spec);
    }

    static bool is_spec_marked_as_only(const SpecInfo& spec)
    {
      return spec.only;
    }

    static void GetSpecsMarkedAsOnly(Specs& specs)
    {
      std::remove_copy_if(GetSpecs().begin(), GetSpecs().end(), std::inserter(specs, specs.end()), is_spec_not_marked_as_only);
    }

    static bool HasSpecsMarkedAsOnly()
    {
      return std::find_if(GetSpecs().begin(), GetSpecs().end(), is_spec_marked_as_only) != GetSpecs().end();
    }

    static void GetSpecsToRun(Specs& specs)
    {
      if(HasSpecsMarkedAsOnly())
      {
        GetSpecsMarkedAsOnly(specs);
      }
      else
      {
        std::copy(GetSpecs().begin(), GetSpecs().end(), std::inserter(specs, specs.end()));
      }
    }
  };  
}
#endif
