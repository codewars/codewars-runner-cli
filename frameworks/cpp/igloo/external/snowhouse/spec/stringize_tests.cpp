
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>
using namespace igloo;

namespace
{
  // No overload for operator<<(std::ostream&) or specialization of igloo::Stringizer
  struct WithoutStreamOperator
  {
    WithoutStreamOperator(int id)
    : m_id(id)
    {
    }

    bool operator==(const WithoutStreamOperator& rhs) const
    {
      return m_id == rhs.m_id;
    }

    int m_id;
  };

  // Has operator<<(std::ostream&)
  struct WithStreamOperator : public WithoutStreamOperator
  {
    WithStreamOperator(int id)
    : WithoutStreamOperator(id)
    {
    }
  };
   
  std::ostream& operator<<(std::ostream& stream, const WithStreamOperator& a)
  {
    stream << a.m_id;
    return stream;
  }

  // Has no operator<<(std::ostream&), but a specialization of igloo::Stringizer
  struct WithoutStreamOperatorButWithStringizer : public WithoutStreamOperator
  {
    WithoutStreamOperatorButWithStringizer(int id)
    : WithoutStreamOperator(id)
    {
    }
  };
}

namespace snowhouse {

  template<>
  struct Stringizer< WithoutStreamOperatorButWithStringizer >
  {
    static std::string ToString(const WithoutStreamOperatorButWithStringizer& value)
    {
      return snowhouse::Stringize(value.m_id);
    }
  };
}

Context(StringizeTests)
{
  Spec(ShouldHandleTypesWithStreamOperators)
  {
    WithStreamOperator a(12);
    WithStreamOperator b(13);
    AssertTestFails(Assert::That(a, Is().EqualTo(b)), "Expected: equal to 13\nActual: 12");
  }

  Spec(ShouldHandleTypesWithoutStreamOperators)
  {
    WithoutStreamOperator a(12);
    WithoutStreamOperator b(13);
    AssertTestFails(Assert::That(a, Is().EqualTo(b)), "Expected: equal to [unsupported type]\nActual: [unsupported type]");
  }

  Spec(ShouldHandleTypesWithTraits)
  {
    WithoutStreamOperatorButWithStringizer a(12);
    WithoutStreamOperatorButWithStringizer b(13);
    AssertTestFails(Assert::That(a, Is().EqualTo(b)), "Expected: equal to 13\nActual: 12");
  }
}; 

Context(StringizeTestsExpressionTemplates)
{
  Spec(ShouldHandleTypesWithStreamOperators)
  {
    WithStreamOperator a(12);
    WithStreamOperator b(13);
    AssertTestFails(Assert::That(a, Equals(b)), "Expected: equal to 13\nActual: 12");
  }

  Spec(ShouldHandleTypesWithoutStreamOperators)
  {
    WithoutStreamOperator a(12);
    WithoutStreamOperator b(13);
    AssertTestFails(Assert::That(a, Equals(b)), "Expected: equal to [unsupported type]\nActual: [unsupported type]");
  }

  Spec(ShouldHandleTypesWithTraits)
  {
    WithoutStreamOperatorButWithStringizer a(12);
    WithoutStreamOperatorButWithStringizer b(13);
    AssertTestFails(Assert::That(a, Is().EqualTo(b)), "Expected: equal to 13\nActual: 12");
  }
}; 

