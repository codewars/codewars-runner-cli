# Not using a module for security reasons,
# See https://github.com/Codewars/codewars-runner/commit/06a7c5a928c1a72c2e119921dcbfec71ed28ba1f#commitcomment-6903818
class test:
    @staticmethod
    def formatMessage(message):
        return message.replace("\n", "\\n")

    @staticmethod
    def expect(passed=None, message=None):
        if passed:
            if message:
                print "<PASSED::>Test Passed: {0}".format(message)
            else:
                print "<PASSED::>Test Passed"

        else:
            message = message or "Value is not what was expected"
            print "<FAILED::>{0}".format(message)
            raise Exception(message)

    @staticmethod
    def assert_equals(actual, expected, message=None):
        expect(actual == expected, message)

    @staticmethod
    def assert_not_equals(actual, expected, message=None):
        expect(actual != expected, message)

    @staticmethod
    def expect_error(message, function):
        passed = false
        try:
            function()
        except Exception:
            passed = true

        expect(passed, message)

    @staticmethod
    def describe(message):
        print "<DESCRIBE::>{0}".format(formatMessage(message))

    @staticmethod
    def it(message):
        print "<IT::>{0}".format(formatMessage(message))
