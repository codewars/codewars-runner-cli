require_relative 'common'
require 'active_support/core_ext/object/blank'
require "pty"

def read_file(file, delete = false)
  if File.exists?(file)
    File.read(file).tap do
      `rm -rf #{file}` if delete
    end
  end
end

$solution = read_file('/workspace/solution.txt') || read_file('/workspace/solution.sh')
$shell ||= ENV['SHELL'] || 'bash'
`mkdir -p /workspace/shell`
Dir.chdir '/workspace/shell'

def run_shell(script: $solution, file: nil, cwd: $cwd, output: true, args: [])
  script = read_file(file) if file
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

  out = out.join.strip
  if output
    Display.print('LOG', out, mode: 'SHELL', label: "Shell Output")
  end

  # remove any stderr formatted output
  out.gsub(/<span class='stderr'>([\s\S]*)<\/span>/, '\1')
end

def compare_with(expected, actual: run_shell, label: 'Solution', &block)
  describe label do
    let(:actual) { actual }
    let(:expected) { expected }

    instance_eval(&block) if block

    it "should have the same output as the expected solution" do
      expect(actual).to eq expected
    end
  end
end

