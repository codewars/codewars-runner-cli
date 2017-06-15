require "spec"

module Spec
  def self.format_message(line)
    line.gsub("\n", "<:LF:>")
  end

  def self.example(msg, mode = "")
    puts format_message("\n<LOG:#{mode.upcase}:Example>#{msg}")
  end

  def self.log(msg, mode = "", label = "")
    puts format_message("\n<LOG:#{mode.upcase}:#{label}>#{msg}")
  end

  class RootContext
    def print_results(elapsed_time)
      puts "\n<COMPLETEDIN::>#{Spec.to_human(elapsed_time)}"
    end
  end

  class CwFormatter < Formatter
    def initialize

    end

    def push(context)
      puts "\n<DESCRIBE::>#{Spec.format_message(context.description)}"
    end

    def pop
      puts "\n<COMPLETEDIN::>"
    end

    def before_example(description)
      puts "\n<IT::>#{Spec.format_message(description)}"
    end

    def report(result)
      ex = if result.exception.is_a?(AssertionFailed)
        result.exception.as(AssertionFailed)
      else
        result
      end

      source_line = Spec.read_line(ex.file, ex.line) rescue nil

      msg = Spec.format_message(source_line ? source_line.strip : result.description)

      case result.kind
        when :success
          puts "\n<PASSED::>Test Passed"
        when :fail
          puts "\n<FAILED::>Test Failed: #{msg}"

        when :error
          puts "\n<ERROR::>#{msg}"
      end

      if result.exception
        puts "\n<ERROR::>#{Spec.format_message(result.exception.to_s)}"
      end
      puts "\n<COMPLETEDIN::>"
    end

    def finish
    end
  end
end

Spec.formatters[0] = Spec::CwFormatter.new
# Spec.override_default_formatter(Spec::CwFormatter.new)
