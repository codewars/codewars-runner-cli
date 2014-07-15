import unittest
import string

def exit():
  return

def formatMessage(message):
  return string.replace(message, "\n", "<:LF:>")

class CwTestResult(unittest.TestResult):
  def addSuccess(self, test):
    print "<PASSED::>Test Passed"
    super(CwTestResult, self).addSuccess(test)

  def addFailure(self, test, err):
    print "<FAILED::>" + formatMessage(err[1].message)
    super(CwTestResult, self).addFailure(test, err)

  def addError(self, test, err):
    print "<ERROR::>Unhandled Exception: "  + formatMessage(err[1].message)
    super(CwTestResult, self).addError(test, error)


