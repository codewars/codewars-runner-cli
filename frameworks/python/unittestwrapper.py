from __future__ import print_function
import unittest

def formatMessage(message):
  return "{0}".format(message).replace("\n", "<:LF:>")

class CwTestResult(unittest.TestResult):

  # def startTestRun(self):
  #   print("<DESCRIBE::>Tests")
  #   super(CwTestResult, self).startTestRun()
  #
  # def stopTestRun(self):
  #   print("<COMPLETEDIN::>")
  #   super(CwTestResult, self).stopTestRun()

  def startTest(self, test):
    print("<IT::>" + test._testMethodName)
    super(CwTestResult, self).startTest(test)

  def stopTest(self, test):
    print("<COMPLETEDIN::>")
    super(CwTestResult, self).stopTest(test)

  def addSuccess(self, test):
    print("<PASSED::>Test Passed")
    super(CwTestResult, self).addSuccess(test)

  def addFailure(self, test, err):
    print("<FAILED::>" + formatMessage(err[1]))
    super(CwTestResult, self).addFailure(test, err)

  def addError(self, test, err):
    print("<ERROR::>Unhandled Exception: " + formatMessage(err[1]))
    super(CwTestResult, self).addError(test, err)
