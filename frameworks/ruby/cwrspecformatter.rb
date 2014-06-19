class CwRSpecFormatter
  def initialize(output)
    @output = output
  end
  def formatMessage(s)
    s.gsub("\n", "\\n")
  end
  RSpec::Core::Formatters.register self, :example_group_started, :example_started, :example_passed, :example_failed, :message
  def example_group_started(notification)
    @output.puts "<DESCRIBE::>" + formatMessage(notification.group.description)
  end
  def example_started(notification)
    @output.puts "<IT::>" + formatMessage(notification.example.description)
  end
  def example_passed(notification)
    @output.puts "<PASSED::>Test Passed"# + formatMessage(notification.example.description)
  end
  def example_failed(notification)
    @output.puts "<FAILED::>" + formatMessage(notification.exception.message) + "\\n" + notification.formatted_backtrace.join("\\n")
  end
  def message(notification)
    @output.puts formatMessage(notification.message)
  end
end
