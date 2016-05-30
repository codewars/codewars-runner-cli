
//          Copyright Joakim Karlsson & Kim Gräsman 2010-2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <tests/igloo_self_test.h>
using namespace igloo;
Context(StringLineTests)
{
  Spec(CanAssertThatAtLeastOneLineInAStreamMatches)
  {
    Assert::That("First line\n", Has().AtLeast(1).EqualTo("First line"));
  }

  Spec(CanDetectWhenAssertionFails)
  {
    AssertTestFails(Assert::That("First line\n", Has().AtLeast(1).EqualTo("Second line")), "Expected: at least 1 equal to Second line");
  }

  Spec(CanHandleLineMissingNewline)
  {
    Assert::That("First line", Has().AtLeast(1).EqualTo("First line"));
  }

  Spec(CanHandleSeveralLines)
  {
    std::string lines = "First line\nSecond line";
    Assert::That(lines, Has().Exactly(2).EndingWith("line"));
  }

  Spec(CanHandleWindowsLineEndings)
  {
    std::string lines = "First line\r\nSecond line\r\nThird line";
    Assert::That(lines, Has().Exactly(3).EndingWith("line"));
  }

  Spec(CanMatchBeginningOfLinesWithWindowsLineEndings)
  {
    std::string lines = "First line\nSecond line\r\nThird line";
    Assert::That(lines, Has().Exactly(1).StartingWith("Second"));
  }

  Spec(CanHandleEmptyLinesWhenUsingWindowsLineEndings)
  {
    std::string lines = "\r\nSecond line\r\n\r\n";
    Assert::That(lines, Has().Exactly(2).OfLength(0));
  }

  Spec(CanHandleLastLineMissingNewlineForWindowsLineEndings)
  {
    std::string lines = "First line\r\nSecond line";
    Assert::That(lines, Has().Exactly(2).EndingWith("line"));
  }

  Spec(CanHandleAllEmptyLines)
  {
    Assert::That("\n\n\n\n\n\n", Has().Exactly(6).OfLength(0));
  }

  Spec(CanHandleAllEmptyLinesWithWindowsLineEndings)
  {
    Assert::That("\r\n\r\n\r\n", Has().Exactly(3).OfLength(0));
  }
};

Context(StringLineParserTests)
{
  Spec(CanParseEmptyString)
  {
    std::vector<std::string> res;

    StringLineParser::Parse("", res);

    Assert::That(res, HasLength(0));
  }

  Spec(CanParseSingleLine)
  {
    std::vector<std::string> res;

    StringLineParser::Parse("Simple line", res);

    Assert::That(res, HasLength(1));
    Assert::That(res, Has().Exactly(1).EqualTo("Simple line"));
  }

  Spec(CanParseTwoLines)
  {
    std::vector<std::string> res;

    StringLineParser::Parse("One line\nTwo lines", res);

    Assert::That(res, HasLength(2));
    Assert::That(res, Has().Exactly(1).EqualTo("One line"));
    Assert::That(res, Has().Exactly(1).EqualTo("Two lines"));
  }

  Spec(CanParseThreeLines)
  {
    std::vector<std::string> res;

    StringLineParser::Parse("One line\nTwo lines\nThree lines", res);

    Assert::That(res, HasLength(3));
    Assert::That(res, Has().Exactly(1).EqualTo("One line"));
    Assert::That(res, Has().Exactly(1).EqualTo("Two lines"));
    Assert::That(res, Has().Exactly(1).EqualTo("Three lines"));
  }

  Spec(CanHandleStringEndingWithNewline)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("One line\n", res);
    Assert::That(res, HasLength(1));
    Assert::That(res, Has().Exactly(1).EqualTo("One line"));
  }

  Spec(CanHandleSingleLineWithWindowsLineEnding)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("One line\r\n", res);
    Assert::That(res, HasLength(1));
    Assert::That(res, Has().Exactly(1).EqualTo("One line"));
  }

  Spec(CanHandleTwoLinesWithWindowsLineEndings)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("One line\r\nTwo lines", res);
    Assert::That(res, HasLength(2));
    Assert::That(res, Has().Exactly(1).EqualTo("One line"));
    Assert::That(res, Has().Exactly(1).EqualTo("Two lines"));
  }

  Spec(CanHandleEmptyLineWithNewline)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("\n", res);
    Assert::That(res, Is().OfLength(1).And().Exactly(1).OfLength(0));
  }

  Spec(CanHandleTwoEmptyLines)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("\n\n", res);
    Assert::That(res, HasLength(2));
    Assert::That(res, Has().Exactly(2).OfLength(0));
  }

  Spec(CanHandleTwoEmptyLinesWithWindowsLineEndings)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("\r\n\r\n", res);
    Assert::That(res, HasLength(2));
    Assert::That(res, Has().Exactly(2).OfLength(0));
  }

  Spec(CanHandleCarriageReturnOnly)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("One line\rTwo lines", res);
    Assert::That(res, HasLength(2));
    Assert::That(res, Has().Exactly(1).EqualTo("One line"));
    Assert::That(res, Has().Exactly(1).EqualTo("Two lines"));
  }

  Spec(CanHandleCarriageReturnOnlyAtEndOfString)
  {
    std::vector<std::string> res;
    StringLineParser::Parse("One line\r\nTwo lines\r", res);
    Assert::That(res, HasLength(2));
    Assert::That(res, Has().Exactly(1).EqualTo("One line"));
    Assert::That(res, Has().Exactly(1).EqualTo("Two lines"));
  }
};
