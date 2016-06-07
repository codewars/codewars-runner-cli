
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_DEFAULTREGISTRATIONALIAS_H
#define IGLOO_DEFAULTREGISTRATIONALIAS_H

#include <igloo/core/registration.h>

// Default aliases
#define Context(contextName) \
IGLOO_CONTEXT_REGISTRATION(contextName)

#define Context_Only(contextName) \
IGLOO_CONTEXT_REGISTRATION_ONLY(contextName)

#define Context_Skip(contextName) \
IGLOO_CONTEXT_REGISTRATION_SKIP(contextName)

#define ParentContext(contextName) \
  IGLOO_PARENT_CONTEXT_REGISTRATION(contextName)

#define SubContext(contextName, baseContextName) \
IGLOO_SUBCONTEXT_REGISTRATION(contextName, baseContextName)

#define Spec(specName) \
IGLOO_SPEC_REGISTRATION(specName)  

#define Spec_Skip(specName) \
IGLOO_SPEC_REGISTRATION_SKIP(specName)  

#define Spec_Only(specName) \
IGLOO_SPEC_REGISTRATION_ONLY(specName)  

#define ContextAttribute(attributeName, attributeValue) \
IGLOO_CONTEXT_ATTRIBUTE_REGISTRATION(attributeName, attributeValue)

// "Classic" aliases
#define TestFixture(fixtureName) \
IGLOO_CONTEXT_REGISTRATION(fixtureName)

#define TestFixture_Only(contextName) \
IGLOO_CONTEXT_REGISTRATION_ONLY(contextName)

#define TestFixture_Skip(contextName) \
IGLOO_CONTEXT_REGISTRATION_SKIP(contextName)

#define DerivedFixture(fixtureName, baseFixtureName) \
IGLOO_SUBCONTEXT_REGISTRATION(fixtureName, baseFixtureName)

#define TestMethod(methodName) \
IGLOO_SPEC_REGISTRATION(methodName)

#define TestMethod_Skip(methodName) \
IGLOO_SPEC_REGISTRATION_SKIP(specName)  

#define TestMethod_Only(methodName) \
IGLOO_SPEC_REGISTRATION_ONLY(specName)  

#define TestAttribute(attributeName, attributeValue) \
IGLOO_CONTEXT_ATTRIBUTE_REGISTRATION(attributeName, attributeValue)

#endif
