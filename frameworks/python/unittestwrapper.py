import unittest

class CwTestResult(unittest.TestResult):
  def addSuccess(self, test):
    print "<PASSED::>Test Passed"
    super(CwTestResult, self).addSuccess(test)

  def addFailure(self, test, err):
    print "<FAILED::>" + err[1].message
    super(CwTestResult, self).addFailure(test, err)

  def addError(self, test, err):
    print "<FAILED::>Unhandled Exception: "  + err[1].message

