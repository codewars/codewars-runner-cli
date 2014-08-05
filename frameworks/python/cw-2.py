# Not using a module for security reasons,
# See https://github.com/Codewars/codewars-runner/commit/06a7c5a928c1a72c2e119921dcbfec71ed28ba1f#commitcomment-6903818

class AssertException(Exception):
    pass

class Test:
    @staticmethod
    def formatMessage(message):
        return message.replace("\n", "<:LF:>")

    @staticmethod
    def expect(passed=None, message=None, allow_raise=False):
        if passed:
#            if message:
#                print "<PASSED::>Test Passed: {0}".format(message)
#            else:
             print "<PASSED::>Test Passed"

        else:
            message = message or "Value is not what was expected"
            print "<FAILED::>{0}".format(message)
            if allow_raise:
              raise AssertException(message)

    @staticmethod
    def assert_equals(actual, expected, message=None, allow_raise=True):
        equals_msg = "{0} should equal {1}".format(repr(actual), repr(expected))
        if message == None:
            message = equals_msg
        else:
            message += ": " + equals_msg
          
        test.expect(actual == expected, message, allow_raise)

    @staticmethod
    def assert_not_equals(actual, expected, message=None, allow_raise=True):
        equals_msg = "{0} should not equal {1}".format(repr(actual), repr(expected))
        if message == None:
            message = equals_msg
        else:
            message += ": " + equals_msg
            
        test.expect(actual != expected, message, allow_raise)

    @staticmethod
    def expect_error(message, function):
        passed = False
        try:
            function()
        except Exception:
            passed = True

        test.expect(passed, message)

    @staticmethod
    def describe(message):
        print "<DESCRIBE::>{0}".format(test.formatMessage(message))

    @staticmethod
    def it(message):
        print "<IT::>{0}".format(test.formatMessage(message))

# simulate a more idiomatic Python approach by having a lower case variant.
test = Test
