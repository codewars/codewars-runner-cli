
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>

using namespace igloo;

const char* ExpectedActual = "\nActual: [ 1, 2, 3, 5, 8 ]";

template <typename T>
struct SequenceContainerTestsBase : public ContextProvider<SequenceContainerTestsBase<T>, IGLOO_CURRENT_CONTEXT, IGLOO_ROOT_CONTEXT, false, false>
{
  typedef SequenceContainerTestsBase<T> IGLOO_CURRENT_CONTEXT;

  void SetUp()
  {
    container.clear();
    container.push_back(1);
    container.push_back(2);
    container.push_back(3);
    container.push_back(5);
    container.push_back(8);
  }

  Spec(ShouldHandleAllOperator)
  {
    Assert::That(container, Has().All().GreaterThan(1).Or().LessThan(4));
  }

  Spec(ShouldHandleFailingAllOperator)
  {
    AssertTestFails(Assert::That(container, Has().All().GreaterThan(4)), std::string("Expected: all greater than 4") + ExpectedActual);
  }

  Spec(SHouldHandleInvalidExpressionAfterAllOperator)
  {
    AssertTestFails(Assert::That(container, Has().All().Not()), "The expression contains a not operator without any operand");
  }

  Spec(ShouldHandleNoExpressionAfterAllOperator)
  {
    AssertTestFails(Assert::That(container, Has().All()), "The expression after \"all\" operator does not yield any result");
  }

  Spec(ShouldHandleAtLeastOperator)
  {
    Assert::That(container, Has().AtLeast(1).LessThan(5));
  }

  Spec(ShouldHandleFailingAtLeastOperator)
  {
    AssertTestFails(Assert::That(container, Has().AtLeast(2).LessThan(2)), std::string("Expected: at least 2 less than 2") + ExpectedActual);
  }

  Spec(ShouldHandleExactlyOperator)
  {
    Assert::That(container, Has().Exactly(1).EqualTo(3));
  }

  Spec(ShouldHandleFailingExactlyOperator)
  {
    AssertTestFails(Assert::That(container, Has().Exactly(2).EqualTo(3)), std::string("Expected: exactly 2 equal to 3") + ExpectedActual);
  }

  Spec(ShouldHandleAtMostOperator)
  {
    Assert::That(container, Has().AtMost(1).EqualTo(5));
  }

  Spec(ShouldHandleFailingAtMostOperator)
  {
    AssertTestFails(Assert::That(container, Has().AtMost(1).EqualTo(3).Or().EqualTo(5)), std::string("Expected: at most 1 equal to 3 or equal to 5") + ExpectedActual);
  }

  Spec(ShouldHandleNoneOperator)
  {
    Assert::That(container, Has().None().EqualTo(666));
  }

  Spec(ShouldHandleFailingNoneOperator)
  {
    AssertTestFails(Assert::That(container, Has().None().EqualTo(5)), std::string("Expected: none equal to 5") + ExpectedActual);
  }

  Spec(ShouldHandleContaining)
  {
    Assert::That(container, Contains(3));
  }

  Spec(ShouldDetectFailingContains)
  {
    AssertTestFails(Assert::That(container, Contains(99)), std::string("contains 99") + ExpectedActual);
  }

  Spec(ShouldHandleOfLength)
  {
    Assert::That(container, HasLength(5));
  }

  Spec(ShouldHandleFailingOfLength)
  {
    AssertTestFails(Assert::That(container, HasLength(7)), std::string("of length 7") + ExpectedActual);
  }

  Spec(ShouldHandleContaining_ExpressionTemplates)
  {
    Assert::That(container, Contains(3));
  }

  Spec(ShouldDetectFailingContains_ExpressionTemplates)
  {
    AssertTestFails(Assert::That(container, Contains(99)), std::string("contains 99") + ExpectedActual);
  }

  Spec(ShouldHandleOfLength_ExpressionTemplates)
  {
    Assert::That(container, HasLength(5));
  }

  Spec(ShouldHandleFailingOfLengthForVectors)
  {
    AssertTestFails(Assert::That(container, HasLength(7)), std::string("of length 7") + ExpectedActual);
  }

  Spec(ShouldHandleIsEmpty)
  {
    T is_empty;

    Assert::That(is_empty, IsEmpty());
  }

  Spec(ShouldHandleFailingIsEmpty)
  {
    AssertTestFails(Assert::That(container, IsEmpty()), "of length 0");
  }

  Spec(ShouldHandleFluentIsEmpty)
  {
    T is_empty;

    Assert::That(is_empty, Is().Empty());
  }

  Spec(ShouldHandleFailingFluentIsEmpty)
  {
    AssertTestFails(Assert::That(container, Is().Empty()), "of length 0");
  }

  Spec(ShouldHandlerEqualsContainer)
  {
    std::list<int> expected;
    expected.assign(container.begin(), container.end());

    AssertThat(container, EqualsContainer(expected));
  }

  Spec(ShouldHandleEqualsContainer_Fluent)
  {
    std::list<int> expected;
    expected.assign(container.begin(), container.end());

    AssertThat(container, Is().EqualToContainer(expected));
  }

  Spec(ShouldHandleFailingEqualsContainer)
  {
    const int e[] = {4, 2, 4};
    std::list<int> expected(e, e + sizeof(e) / sizeof(e[0]));

    AssertTestFails(Assert::That(container, EqualsContainer(expected)), "Expected: [ 4, 2, 4 ]");
  }

  Spec(ShouldHandleFailingEqualsContainer_Fluent)
  {
    const int e[] = {4, 2, 4};
    std::list<int> expected(e, e + sizeof(e) / sizeof(e[0]));

    AssertTestFails(Assert::That(container, Is().EqualToContainer(expected)), "Expected: [ 4, 2, 4 ]");
  }

  T container;
};

SubContext(VectorTests, SequenceContainerTestsBase<std::vector<int> >)
{
};

SubContext(ListTests, SequenceContainerTestsBase<std::list<int> >)
{
};

SubContext(DequeTests, SequenceContainerTestsBase<std::deque<int> >)
{
};
