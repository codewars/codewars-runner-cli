

$describing = false

class Test
  class << self
    def log(message, no_line_break = false)
      if $describing
        message = message.to_s
        @@html << message
        @@html << '<br>' unless no_line_break

      else
        CW_OUTPUT << message.to_s
      end
      nil
    end
  end
end

