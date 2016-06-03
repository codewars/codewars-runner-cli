
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_ALTERNATIVEREGISTRATIONALIASES_H
#define IGLOO_ALTERNATIVEREGISTRATIONALIASES_H

#include <igloo/core/registration.h>

// Spec aliases
#define Describe(contextName) \
IGLOO_CONTEXT_REGISTRATION(contextName)

#define Describe_Only(contextName) \
IGLOO_CONTEXT_REGISTRATION_ONLY(contextName)

#define Describe_Skip(contextName) \
IGLOO_CONTEXT_REGISTRATION_SKIP(contextName)

#define It(specName) \
IGLOO_SPEC_REGISTRATION(specName)  

#define It_Skip(specName) \
IGLOO_SPEC_REGISTRATION_SKIP(specName)  

#define It_Only(specName) \
IGLOO_SPEC_REGISTRATION_ONLY(specName)  

#define DescriptionAttribute(attributeName, attributeValue) \
IGLOO_CONTEXT_ATTRIBUTE_REGISTRATION(attributeName, attributeValue)

// Behavior aliases
#define When(contextName) \
IGLOO_CONTEXT_REGISTRATION(contextName)

#define When_Only(contextName) \
IGLOO_CONTEXT_REGISTRATION_ONLY(contextName)

#define When_Skip(contextName) \
IGLOO_CONTEXT_REGISTRATION_SKIP(contextName)

#define Then(specName) \
IGLOO_SPEC_REGISTRATION(specName)  

#define Then_Skip(specName) \
IGLOO_SPEC_REGISTRATION_SKIP(specName)  

#define Then_Only(specName) \
IGLOO_SPEC_REGISTRATION_ONLY(specName)  

#define ScenarioAttribute(attributeName, attributeValue) \
IGLOO_CONTEXT_ATTRIBUTE_REGISTRATION(attributeName, attributeValue)

#endif
