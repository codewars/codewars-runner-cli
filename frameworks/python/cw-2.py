from __future__ import print_function

class AssertException(Exception):
    pass


_print = print


'''Fix the dreaded Unicode Error Trap'''
def print(*args, **kwargs):
    from sys import stdout
    sep = kwargs.get('sep', ' ')
    end = kwargs.get('end', '\n')
    file = kwargs.get('file', stdout)
    
    def _replace(c):
        if ord(c) >= 128: return u'&#{};'.format(ord(c))
        return c
    def _escape(s): return ''.join(_replace(c) for c in s)
    
    _print(*map(_escape, args), sep=_escape(sep), end=_escape(end), file=file)


def format_message(message):
    def _replace(c):
        if ord(c) >= 65536: return r'\U' + hex(ord(c))[2:].zfill(8)
        if ord(c) >= 128: return r'\u' + hex(ord(c))[2:].zfill(4)
        return c
    def _escape(s): return ''.join(_replace(c) for c in s)
    return _escape(message.replace("\n", "<:LF:>"))


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


'''
Usage:
@describe('describe text')
def describe1():
    @it('it text')
    def it1():
        # some test cases...
'''
def _timed_block_factory(opening_text):
    from timeit import default_timer as timer
    from traceback import format_exception
    from sys import exc_info
    
    def _timed_block_decorator(s, before=None, after=None):
        display(opening_text, s)
        def wrapper(func):
            if callable(before): before()
            time = timer()
            try: func()
            except:
                fail('Unexpected exception raised')
                tb_str = ''.join(format_exception(*exc_info()))
                display('ERROR', tb_str)
            display('COMPLETEDIN', '{:.2f}'.format((timer() - time) * 1000))
            if callable(after): after()
        return wrapper
    return _timed_block_decorator

describe = _timed_block_factory('DESCRIBE')
it = _timed_block_factory('IT')


'''
Timeout utility
Usage:
@timeout(sec)
def some_tests():
    any code block...
Note: Timeout value can be a float.
'''
def timeout(sec):
    def wrapper(func):
        from multiprocessing import Process
        process = Process(target=func)
        process.start()
        process.join(sec)
        if process.is_alive():
            fail('Exceeded time limit of {:.3f} seconds'.format(sec))
            process.terminate()
            process.join()
    return wrapper
