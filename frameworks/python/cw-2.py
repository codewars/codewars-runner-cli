from __future__ import print_function

class AssertException(Exception):
    pass


def format_message(message):
    return message.replace("\n", "<:LF:>")


def display(type, message, label="", mode=""):
    print("\n<{0}:{1}:{2}>{3}".format(type.upper(), mode.upper(), label, format_message(message)))


def expect(passed=None, message=None, allow_raise=False):
    if passed:
        display('PASSED', 'Test Passed')
    else:
        message = message or "Value is not what was expected"
        display('FAILED', message)
        if allow_raise:
            raise AssertException(message)


def assert_equals(actual, expected, message=None, allow_raise=False):
    equals_msg = "{0} should equal {1}".format(repr(actual), repr(expected))
    if message is None:
        message = equals_msg
    else:
        message += ": " + equals_msg

    expect(actual == expected, message, allow_raise)


def assert_not_equals(actual, expected, message=None, allow_raise=False):
    equals_msg = "{0} should not equal {1}".format(repr(actual), repr(expected))
    if message is None:
        message = equals_msg
    else:
        message += ": " + equals_msg

    expect(not (actual == expected), message, allow_raise)


def expect_error(message, function):
    passed = False
    try:
        function()
    except:
        passed = True
    expect(passed, message)


def pass_(): expect(True)
def fail(message): expect(False, message)


def assert_approx_equals(actual, expected, margin=1e-9, message=None, allow_raise=False):
    equals_msg = "{0} should be close to {1} with absolute or relative margin of {2}".format(
        repr(actual), repr(expected), repr(margin))
    if message is None: message = equals_msg
    else: message += ": " + equals_msg
    div = max(abs(actual), abs(expected), 1)
    expect(abs((actual - expected) / div) < margin, message, allow_raise)


def describe(message):
    display('DESCRIBE', message)


def it(message):
    display('IT', message)
