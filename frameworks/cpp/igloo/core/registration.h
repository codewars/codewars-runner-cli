
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_REGISTRATION_H
#define IGLOO_REGISTRATION_H

#define IGLOO_PRIVATE_GENERATE_CONTEXTREGISTRAR(contextName, baseContextName) \
  struct contextName; \
  struct ContextRegistrar_##contextName \
  { \
    ContextRegistrar_##contextName() \
    { \
      igloo::TestRunner::RegisterContext<igloo::ContextRunner<baseContextName, contextName> >(#contextName, __FILE__, __LINE__); \
    } \
  } contextName##_IglooRegistrar;

#define IGLOO_CONTEXT_REGISTRATION(contextName) \
  IGLOO_PRIVATE_GENERATE_CONTEXTREGISTRAR(contextName, void) \
  struct contextName : public ContextProvider<contextName, IGLOO_CURRENT_CONTEXT, IGLOO_ROOT_CONTEXT, false, false>

#define IGLOO_CONTEXT_REGISTRATION_ONLY(contextName) \
  IGLOO_PRIVATE_GENERATE_CONTEXTREGISTRAR(contextName, void) \
  struct contextName : public ContextProvider<contextName, IGLOO_CURRENT_CONTEXT, IGLOO_ROOT_CONTEXT, true, false>

#define IGLOO_CONTEXT_REGISTRATION_SKIP(contextName) \
  IGLOO_PRIVATE_GENERATE_CONTEXTREGISTRAR(contextName, void) \
  struct contextName : public ContextProvider<contextName, IGLOO_CURRENT_CONTEXT, IGLOO_ROOT_CONTEXT, false, true>

#define IGLOO_PARENT_CONTEXT_REGISTRATION(contextName) \
  struct contextName : public ContextProvider<contextName, IGLOO_CURRENT_CONTEXT, IGLOO_ROOT_CONTEXT, false, false>

#define IGLOO_SUBCONTEXT_REGISTRATION(contextName, baseContextName) \
  IGLOO_PRIVATE_GENERATE_CONTEXTREGISTRAR(contextName, baseContextName) \
  struct contextName : public baseContextName

#define IGLOO_PRIVATE_SPEC_REGISTRATION(specName, skip, only) \
  struct SpecRegistrar_##specName \
  { \
    SpecRegistrar_##specName() \
    { \
	  ContextRegistry<IGLOO_CURRENT_CONTEXT>::RegisterSpec(#specName, &IGLOO_CURRENT_CONTEXT::specName, skip, only); \
    } \
  } SpecRegistrar_##specName; \
  virtual void specName()
                       
#define IGLOO_SPEC_REGISTRATION(specName) \
  IGLOO_PRIVATE_SPEC_REGISTRATION(specName, false, false)

#define IGLOO_SPEC_REGISTRATION_SKIP(specName) \
  IGLOO_PRIVATE_SPEC_REGISTRATION(specName, true, false)

#define IGLOO_SPEC_REGISTRATION_ONLY(specName) \
  IGLOO_PRIVATE_SPEC_REGISTRATION(specName, false, true)

#define IGLOO_CONTEXT_ATTRIBUTE_REGISTRATION(name, value) \
    struct AttributeRegistrar_##IGLOO_CURRENT_CONTEXT \
    {\
      AttributeRegistrar_##IGLOO_CURRENT_CONTEXT()\
      {\
        ContextAttributeStorage<IGLOO_CURRENT_CONTEXT>::Set(name, value);\
      }\
    } AttributeRegistrar_##IGLOO_CURRENT_CONTEXT;

#endif
