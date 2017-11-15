from __future__ import print_function
import unittest
import traceback

def formatError(err):
  exc_type, exc_value, exc_traceback = err
  lines = traceback.format_exception(exc_type, exc_value, exc_traceback)
  return "{0}".format(lines[-1][:-1]).replace("\n", "<:LF:>")

class CwTestResult(unittest.TestResult):

  # def startTestRun(self):
  #   print("<DESCRIBE::>Tests")
  #   super(CwTestResult, self).startTestRun()
  #
  # def stopTestRun(self):
  #   print("<COMPLETEDIN::>")
  #   super(CwTestResult, self).stopTestRun()

  def startTest(self, test):
    print("\n<IT::>" + test._testMethodName)
    super(CwTestResult, self).startTest(test)

  def stopTest(self, test):
    print("\n<COMPLETEDIN::>")
    super(CwTestResult, self).stopTest(test)

  def addSuccess(self, test):
    print("\n<PASSED::>Test Passed")
    super(CwTestResult, self).addSuccess(test)

  def addFailure(self, test, err):
    print("\n<FAILED::>" + formatError(err))
    super(CwTestResult, self).addFailure(test, err)

  def addError(self, test, err):
    print("\n<ERROR::>" + formatError(err))
    super(CwTestResult, self).addError(test, err)
