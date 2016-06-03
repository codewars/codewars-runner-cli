
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_TESTLISTENER_AGGREGATOR_H
#define IGLOO_TESTLISTENER_AGGREGATOR_H

namespace igloo {
  class TestListenerAggregator : public TestListener
  {
    public:
      void AddListener(TestListener* listener)
      {
        listeners_.push_back(listener);
      }

      void TestRunStarting() 
      {
        for(TestListeners::const_iterator it = listeners_.begin(); it != listeners_.end(); it++)
        {
          (*it)->TestRunStarting();
        }
      }

      void TestRunEnded(const TestResults& results)
      {
        for(TestListeners::const_iterator it = listeners_.begin(); it != listeners_.end(); it++)
        {
          (*it)->TestRunEnded(results);
        }
      }

      void ContextRunStarting(const ContextBase& context)
      {
        for(TestListeners::const_iterator it = listeners_.begin(); it != listeners_.end(); it++)
        {
          (*it)->ContextRunStarting(context);
        }
      }

      void ContextRunEnded(const ContextBase& context)
      {
        for(TestListeners::const_iterator it = listeners_.begin(); it != listeners_.end(); it++)
        {
          (*it)->ContextRunEnded(context);
        }
      }

      void SpecRunStarting(const ContextBase& context, const std::string& specName)
      {
        for(TestListeners::const_iterator it = listeners_.begin(); it != listeners_.end(); it++)
        {
          (*it)->SpecRunStarting(context, specName);
        }
      }

      void SpecSucceeded(const ContextBase& context, const std::string& specName)
      {
        for(TestListeners::const_iterator it = listeners_.begin(); it != listeners_.end(); it++)
        {
          (*it)->SpecSucceeded(context, specName);
        }
      }

      void SpecFailed(const ContextBase& context, const std::string& specName)
      {
        for(TestListeners::const_iterator it = listeners_.begin(); it != listeners_.end(); it++)
        {
          (*it)->SpecFailed(context, specName);
        }
      }

    private:
      typedef std::list<TestListener*> TestListeners;
      TestListeners listeners_;
  };
}

#endif
