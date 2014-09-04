from __future__ import print_function
import unittest

def formatMessage(message):
  return "{0}".format(message).replace("\n", "<:LF:>")

class CwTestResult(unittest.TestResult):
  def addSuccess(self, test):
    print("<PASSED::>Test Passed")
    super(CwTestResult, self).addSuccess(test)

  def addFailure(self, test, err):
    print("<FAILED::>" + formatMessage(err[1]))
    super(CwTestResult, self).addFailure(test, err)

  def addError(self, test, err):
    print("<ERROR::>Unhandled Exception: " + formatMessage(err[1]))
    super(CwTestResult, self).addError(test, err)
