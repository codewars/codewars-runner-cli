import unittest
import traceback
from time import perf_counter

class CodewarsTestRunner(object):
    def __init__(self): pass
    def run(self, test):
        r = CodewarsTestResult()
        s = perf_counter()
        print("\n<DESCRIBE::>Tests")
        try:
            test(r)
        finally:
            pass
        print("\n<COMPLETEDIN::>{:.4f}".format(1000*(perf_counter() - s)))
        return r

__unittest = True
class CodewarsTestResult(unittest.TestResult):
    def __init__(self):
        super().__init__()
        self.start = 0.0

    def startTest(self, test):
        print("\n<IT::>" + test._testMethodName)
        super().startTest(test)
        self.start = perf_counter()

    def stopTest(self, test):
        print("\n<COMPLETEDIN::>{:.4f}".format(1000*(perf_counter() - self.start)))
        super().stopTest(test)

    def addSuccess(self, test):
        print("\n<PASSED::>Test Passed")
        super().addSuccess(test)

    def addError(self, test, err):
        print("\n<ERROR::>Unhandled Exception")
        print("\n<LOG:ESC:Error>" + esc(''.join(traceback.format_exception_only(err[0], err[1]))))
        print("\n<LOG:ESC:Traceback>" + esc(self._exc_info_to_string(err, test)))
        super().addError(test, err)

    def addFailure(self, test, err):
        print("\n<FAILED::>Test Failed")
        print("\n<LOG:ESC:Failure>" + esc(''.join(traceback.format_exception_only(err[0], err[1]))))
        super().addFailure(test, err)

    # from unittest/result.py
    def _exc_info_to_string(self, err, test):
        exctype, value, tb = err
        # Skip test runner traceback levels
        while tb and self._is_relevant_tb_level(tb):
            tb = tb.tb_next
        if exctype is test.failureException:
            length = self._count_relevant_tb_levels(tb) # Skip assert*() traceback levels
        else:
            length = None
        return ''.join(traceback.format_tb(tb, limit=length))

    def _is_relevant_tb_level(self, tb):
        return '__unittest' in tb.tb_frame.f_globals

    def _count_relevant_tb_levels(self, tb):
        length = 0
        while tb and not self._is_relevant_tb_level(tb):
            length += 1
            tb = tb.tb_next
        return length

def esc(s):
    return s.replace("\n", "<:LF:>")
