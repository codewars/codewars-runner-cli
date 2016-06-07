
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>

using namespace igloo;

Context(a_container_with_custom_objects)
{
  struct my_type
  {
    my_type(int my_val)
      : my_val_(my_val)
    {}

    friend bool operator==(const my_type&, const my_type&);
    friend bool operator!=(const my_type&, const my_type&);
    friend std::ostream& operator<<(std::ostream&, const my_type&);

    int my_val_;
  };


  Spec(it_should_be_able_to_compare_containers_of_custom_types)
  {
    const my_type e[] = {my_type(1), my_type(3)};
    const std::list<my_type> expected(e, e + sizeof(e) / sizeof(e[0]));

    AssertThat(my_container_, EqualsContainer(expected));
  }

  Spec(it_should_handle_failing_comparisons)
  {
    const my_type e[] = {my_type(1), my_type(2)};
    const std::list<my_type> expected(e, e + sizeof(e) / sizeof(e[0]));

    AssertTestFails(Assert::That(my_container_, EqualsContainer(expected)), "Expected: [ (my_type: my_val_=1 ), (my_type: my_val_=2 ) ]");
  }

  Spec(it_should_handle_comparison_with_a_predicate_function)
  {
    const my_type e[] = {my_type(1), my_type(3)};
    const std::list<my_type> expected(e, e + sizeof(e) / sizeof(e[0]));

    Assert::That(my_container_, EqualsContainer(expected, are_my_types_equal));
    Assert::That(my_container_, Is().EqualToContainer(expected, are_my_types_equal));
  }

  static bool are_my_types_equal(const my_type& lhs, const my_type& rhs)
  {
    return lhs.my_val_ == rhs.my_val_;
  }

  void SetUp()
  {
    my_container_.push_back(my_type(1));
    my_container_.push_back(my_type(3));
  }

  std::list<my_type> my_container_;
};

bool operator==(const a_container_with_custom_objects::my_type& lhs, const a_container_with_custom_objects::my_type& rhs)
{
  return lhs.my_val_ == rhs.my_val_;
}

bool operator!=(const a_container_with_custom_objects::my_type& lhs, const a_container_with_custom_objects::my_type& rhs)
{
  return !(lhs == rhs);
}

std::ostream& operator<<(std::ostream& stream, const a_container_with_custom_objects::my_type& item)
{
  stream << "(my_type: my_val_=" << item.my_val_ << " )";
  return stream;
}
