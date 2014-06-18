

class Test:
  @staticmethod
  def expect(passed = None, message = None):
    if passed:
      success_msg = "Test Passed"
      #TODO add optional message

      print "<PASSED::>" + success_msg
    else:
      message = message or "Value is not what was expected"
      print "<FAILED::>" + message

      raise StandardError(message)
      #TODO describing stack maybe


