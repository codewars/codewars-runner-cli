
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_CONTEXTPROVIDER_H_
#define IGLOO_CONTEXTPROVIDER_H_

namespace igloo {
  
  namespace detail {
    //
    // Check if the context is marked as "only"
    //
    template <typename T>
    inline bool IsContextMarkedAsOnly()
    {
      return T::IsContextMarkedAsOnly();
    }

    //
    // Specialization for root context. Is never marked "only".
    //
    template <>
    inline bool IsContextMarkedAsOnly<ContextWithAttribute<void> >()
    {
      return false;
    }

    //
    // Check if the context is marked as "skip"
    //
    template <typename T>
    inline bool IsContextMarkedAsSkip()
    {
      return T::IsMarkedAsSkip();
    }

    //
    // Specialization for root context. Is never marked as "skip".
    //
    template <>
    inline bool IsContextMarkedAsSkip<ContextWithAttribute<void> >()
    {
      return false;
    }
  }

  //
  // This class provides functionality for nested contexts.
  //
  // Template arguments:
  //   InnerContext - this is the type of the current context.
  //   OuterContext - the type of the outer (parent) context. 
  //                  For root contexts, this is 'void'.
  //   ISONLY       - Marks this context as 'only', which means 
  //                  this context and its children are the only
  //                  contexts to be executed.
  //   ISSKIP       - Marks this context as 'skip', which means
  //                  this context and its children should not
  //                  be executed.
  //
  template <typename InnerContext, typename OuterContext, bool ISONLY, bool ISSKIP>
    struct NestedContextProvider: public igloo::ContextWithAttribute<InnerContext>
  {
    //
    // By storing the current context in 'IGLOO_CURRENT_CONTEXT', the macros for
    // registering contexts nested to this will be created with this context as
    // outer context.
    //
    typedef InnerContext IGLOO_CURRENT_CONTEXT;

    static bool IsContextMarkedAsOnly()
    {
      return ISONLY || 
             ContextRegistry<InnerContext>::HasSpecsMarkedAsOnly() || 
             detail::IsContextMarkedAsOnly<OuterContext>();
    }

    static bool IsMarkedAsSkip()
    {
      return ISSKIP || detail::IsContextMarkedAsSkip<OuterContext>();
    }

    virtual OuterContext& Parent()
    {
      if(m_outerContext.get() == 0)
      {
        m_outerContext = std::auto_ptr<OuterContext>(CreateIglooContext<OuterContext>());
      }
      return *(m_outerContext.get());
    }

    virtual void IglooFrameworkSetUp()
    {
      Parent().IglooFrameworkSetUp();
      this->SetUp();
    }

    virtual void IglooFrameworkTearDown()
    {
      this->TearDown();
      Parent().IglooFrameworkTearDown();
    }

    private:
    template <typename ContextType>
      ContextType* CreateIglooContext()
      {
        return new ContextType();
      }

    std::auto_ptr<OuterContext> m_outerContext;
  };

  //
  // This class makes it possible for contexts to retrieve the root context.
  //
  template <typename InnerContext, typename OuterContext, typename RootContext, bool ISONLY, bool ISSKIP>
    struct ContextProvider : public NestedContextProvider<InnerContext, OuterContext, ISONLY, ISSKIP>
    {
      //
      // For non root contexts, we ask our parent to return the root context.
      // This will work recursively up to the root context.
      //
      RootContext& Root()
      {
        return NestedContextProvider<InnerContext, OuterContext, ISONLY, ISSKIP>::Parent().Root();
      }
    };

  //
  // Specialization for the root contexts.
  //
  template <typename InnerContext, bool ISONLY, bool ISSKIP>
    struct ContextProvider<InnerContext, ContextWithAttribute<void>, void, ISONLY, ISSKIP> : public NestedContextProvider<InnerContext, ContextWithAttribute<void>, ISONLY, ISSKIP>
  {
      //
      // By setting this, nested contexts will have access to the type
      // of the root context so that their 'Root()' method can return 
      // the correct type.
      //
      typedef InnerContext IGLOO_ROOT_CONTEXT;

      //
      // For the root context, we return a reference to this
      // context.
      //
      InnerContext& Root()
      {
        return *(static_cast<InnerContext*>(this));
      }
  };

}
#endif
