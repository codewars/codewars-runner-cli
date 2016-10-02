# flush output when using rspec to support realtime stdout.
$stdout.sync = true
require 'json'

alias :_puts :puts
def puts(*args)
  if (defined?(Sequel) and args[0].is_a?(Sequel::Dataset))
    _puts Display.table(args[0].to_a)
  else
    _puts *args
  end
end

class Display
  class << self
    def table(data, label: "Data")
      log(data.to_json, label: label)
    end

    def log(msg, mode: "", label: "")
      write('LOG', msg, mode: mode, label: label)
    end

    def write(type, msg, mode: "", label: "")
      puts format_msg("<#{type.upcase}:#{mode.upcase}:#{label}>#{msg}")
    end

    def prop(name, value)
      puts format_msg("<PROP::#{name}>#{value}")
    end

    def format_msg(msg)
      msg.gsub("\n", '<:LF:>')
    end
  end
end