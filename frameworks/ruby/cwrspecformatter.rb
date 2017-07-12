require_relative 'common'

class CwRSpecFormatter
  LF_TOKEN = "<:LF:>"

  def initialize(output)
    @output = output
  end

  def format_message(s)
    s.gsub("\n", LF_TOKEN)
  end

  def format_exception(notification)
    format_message(notification.exception.message.split("\\n/tmp").first)
  end

  def format_backtrace(notification)
    case SpecFormatter.backtrace_format
      when :simple
        trace = notification.formatted_backtrace
            .map {|chunk| chunk.split('`').last.split("'").first }

        if trace.any?
          "#{LF_TOKEN}Simplified backtrace: #{trace.join(' -> ')}"
        else
          ''
        end

      when :detailed
        LF_TOKEN + notification.formatted_backtrace.join(LF_TOKEN)
      end
  end

  RSpec::Core::Formatters.register self, :example_group_started, :example_group_finished, :example_started, :example_passed, :example_failed, :message
  def example_group_started(notification)
    @output.puts "\n<DESCRIBE::>#{format_message(notification.group.description)}"
  end

  def example_group_finished(notification)
    @output.puts "\n<COMPLETEDIN::>"
  end

  def example_started(notification)
    @output.puts "\n<IT::>#{format_message(notification.example.description)}"
  end
  def example_passed(notification)
    @output.puts "\n<PASSED::>Test Passed"# + format_message(notification.example.description)
    @output.puts "\n<COMPLETEDIN::>"
  end
  def example_failed(notification)
    if notification.exception.is_a? RSpec::Expectations::ExpectationNotMetError
      @output.puts "\n<FAILED::>#{format_exception(notification)}"
    else
      @output.puts "\n<ERROR::>#{format_exception(notification)}#{format_backtrace(notification)}"
    end
    @output.puts "\n<COMPLETEDIN::>"
  end
  def message(notification)
    @output.puts format_message(notification.message)
  end
end

# public version
class SpecFormatter
  def self.backtrace_format
    @backtrace_format ||= :simple
  end

  def self.backtrace_format=(value)
    @backtrace_format = value
  end
end
