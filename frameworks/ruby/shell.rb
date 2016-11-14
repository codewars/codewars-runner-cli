require_relative 'common'
require 'active_support/core_ext/object/blank'
require "pty"

$solution = File.read('/home/codewarrior/solution.txt')
$shell ||= 'bash'
`mkdir -p /workspace/shell`
Dir.chdir '/workspace/shell'

def run_shell(script: $solution, cwd: $cwd, output: true, args: [])
  out = []

  # use PTY so that we can pass in args and get stdout/err all within one stream
  PTY.spawn( "/bin/#{$shell}", "-c", script, $shell, *args.map(&:to_s) ) do |stdout, stdin, pid|
    begin
      stdout.each do |line|
        if line.start_with?("#{$shell}: ")
          out << "<span class='stderr'>#{line}</span>"
        else
          out << line
        end
      end
    rescue Errno::EIO
    end
  end

  out = out.join
  if output
    Display.print('LOG', out, mode: 'SHELL', label: "Shell Output")
  end

  # remove any stderr formatted output
  out.gsub(/<span class='stderr'>([\s\S]*)<\/span>/, '\1')
end

def compare_with(expected, actual = run_shell, &block)
  describe "Solution" do
    let(:actual) { actual }
    let(:expected) { expected }

    instance_eval(&block) if block

    it "should have the same output as the expected solution" do
      expect(actual).to eq expected
    end
  end
end

