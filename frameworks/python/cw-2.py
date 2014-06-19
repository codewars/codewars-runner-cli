import string

class Test:
  @staticmethod
  def formatMessage(message):
      return string.replace(message, "\n", "\\n")

  @staticmethod
  def expect(passed = None, message = None):
    if passed:
      success_msg = "Test Passed"
      #TODO add optional message

      print "<PASSED::>" + success_msg
    else:
      message = message or "Value is not what was expected"
      print "<FAILED::>" + message

      raise StandardError(message)
      #TODO describing stack maybe

  @staticmethod
  def assert_equals(actual, expected, message = None):
    expect(actual == expected, message)

  @staticmethod
  def assert_not_equals(actual, expected, message = None):
    expect(actual != expected, message)

  def expect_error(message, function):
    passed = false
    try:
      function()
    except Exception:
      passed = true

    expect(passed, message)

  def expect_error(message, function):
    passed = true
    try:
      function()
    except Exception:
      passed = false

    expect(passed, message)

  @staticmethod
  def describe(message):
    print "<DESCRIBE::>" + formatMessage(message)

  @staticmethod
  def it(message):
    print "<IT::>" + formatMessage(message)

