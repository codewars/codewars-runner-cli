require 'json'
require 'date'
require 'forwardable'
require 'delegate'
require 'set'
require 'uri'
require_relative 'common'

NameError
class Test

  class Error < StandardError
  end

  @@html = []
  @@method_calls = {}
  @@failed = []
  @@before_blocks = []
  @@after_blocks = []
  $describing = false

  class << self

    def expect(passed = nil, message = nil, options = {}, &block)
      success = (block_given? ? block.call() : !!passed)

      if success
        success_msg = "Test Passed"
        success_msg += ": #{options[:success_msg]}" if options[:success_msg]

        puts "\n<PASSED::>#{success_msg}"
      else
        message ||= 'Value is not what was expected'
        puts "\n<FAILED::>#{message}"
        if $describing
          @@failed << Test::Error.new(message)
        else
          raise Test::Error, (message)
        end
      end

    end

    def measure
      start = Time.now
      yield
      ((Time.now - start) * 1000).to_i
    end

    def describe(message, &block)
      ms = measure do
        begin
          $describing = true
          puts "\n<DESCRIBE::>#{message}"
          yield
        ensure
          $describing = false
          @@before_blocks.clear
          @@after_blocks.clear
          raise @@failed.first if @@failed.any?
        end
      end
      puts "\n<COMPLETEDIN::>#{ms}ms" if ms
    end

    def it(message, &block)
      puts "\n<IT::>#{message}"
      @@before_blocks.each do |block|
        block.call
      end
      begin
        wrap_error(&block)
      ensure
        @@after_blocks.each do |block|
          block.call
        end
        puts "\n<COMPLETEDIN::>"
      end
    end

    def before(&block)
      @@before_blocks << block
    end

    def after(&block)
      @@after_blocks << block
    end

    def expect_error(message = nil, &block)
      passed = false
      begin
        block.call
      rescue
        passed = true
      end

      Test.expect(passed, message || 'Expected an error to be raised')
    end

    def expect_no_error(message = nil, &block)
      begin
        block.call
        Test.expect(true)
      rescue Test::Error => test_ex
      rescue => ex
        message ||= 'Unexpected error was raised.'
        Test.expect(false, "#{message}: #{ex.message}")
      end
    end

    def assert_equals(actual, expected, msg = nil, options = {})
      if actual != expected
        msg = msg ? msg + ' -  ' : ''
        message = "#{msg}Expected: #{expected.inspect}, instead got: #{actual.inspect}"
        Test.expect(false, message)
      else
        options[:success_msg] ||= 'Value == ' + expected.inspect
        Test.expect(true, nil, options)
      end
    end

    def assert_not_equals(actual, expected, msg = nil, options = {})
      if actual == expected
        msg = msg ? msg + ' - ' : ''
        message = "#{msg}Not expected: #{actual.inspect}"
        Test.expect(false, message)
      else
        options[:success_msg] ||= "Value != #{expected.inspect}"
        Test.expect(true, nil, options)
      end
    end

    def assert_include(actual, expected, options = {})
      if actual.include?(expected)
        options[:success_msg] ||= 'Value included ' + expected.inspect
        Test.expect(true, nil, options)
      else
        msg = msg ? msg + ' -  ' : ''
        message = "#{msg}Expected: #{actual.inspect} to include #{expected.inspect}"
        Test.expect(false, message)
      end
    end

    def assert_not_include(actual, expected, options = {})
      if !actual.include?(expected)
        options[:success_msg] ||= 'Value did not include ' + expected.inspect
        Test.expect(true, nil, options)
      else
        msg = msg ? msg + ' -  ' : ''
        message = "#{msg}Expected: #{actual.inspect} to not include #{expected.inspect}"
        Test.expect(false, message)
      end
    end

    def fail(msg = nil)
      Test.expect(false, msg)
    end

    def random_letter
      ('a'..'z').to_a.sample
    end

    def random_token
      rand(36**6).to_s(36)
    end

    def random_number
      rand(100)
    end

    def format_msg(*args)
      Display.format_msg(*args)
    end

    private


    def wrap_error
      begin
        yield
      rescue Test::Error => test_ex
        # Do nothing, output should have already been written.
      rescue => ex
        handle_error(ex)
      end
    end

    def handle_error(ex)
      if ex.is_a? Exception
        puts "\n<ERROR::>#{Display.format_msg(ex.inspect)}<:LF:>#{ex.backtrace.join('<:LF:>')}"
      else
        puts "\n<ERROR::>#{Display.format_msg(ex)}"
      end
    end

  end

  Test.freeze
end

def describe(message, &block)
  Test.describe(message, &block)
end

def it(message, &block)
  Test.it(message, &block)
end

def before(&block)
  Test.before(&block)
end

def after(&block)
  Test.after(&block)
end
