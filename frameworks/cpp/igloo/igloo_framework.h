
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef IGLOO_IGLOO_FRAMEWORK_H
#define IGLOO_IGLOO_FRAMEWORK_H

#define IGLOO_VERSION "1.1.1"

#include <iostream>
#include <map>
#include <vector>
#include <sstream>
#include <stack>
#include <list>
#include <memory>
#include <algorithm>
#include <iterator>

#include <igloo/external/snowhouse/snowhouse/snowhouse.h>

namespace igloo {
  using namespace snowhouse;
}

#include <igloo/core/core.h>

//
// These typedefs are used when registering contexts. For root
// contexts, these are the types available. When a context is
// registered it will redefine these so that any nested contexts
// defined within will know about its outer context. We can also
// use template specialization for contexts with the types here
// to get different behaviors for root and nested contexts.
//
typedef igloo::ContextWithAttribute<void> IGLOO_CURRENT_CONTEXT;

//
// This is the current root context (void to begin with).
//
typedef void IGLOO_ROOT_CONTEXT;


#endif
