require_relative 'common'
require 'sequel'
require "faker"
require 'active_support/core_ext/object/blank'

$sql = File.read('/home/codewarrior/solution.txt')
$sql_cleaned = $sql.gsub(/(\/\*([\s\S]*?)\*\/|--.*)/, "")
$sql_commands = $sql_cleaned.split(/; *$/).select(&:present?)

def run_sql(limit: 100, print: true)
  results = $sql_commands.map do |cmd|
    result = cmd.downcase.start_with?("insert") ? DB.prepare(cmd) : DB[cmd]
    result.to_a.tap do |results|
      if results.any? or $sql_commands.length == 1
        Display.table(results.take(limit), label: "SQL Results") if print
        if results.count > limit
          puts "Truncated #{results.count} results to show only top 100"
        end
      end
    end
  end.select {|r| r.length > 0 }

  $sql_results = results.length > 1 ? results : results.first

rescue Sequel::DatabaseError => ex
  msg = ex.message.gsub("SQLite3::SQLException: ", "");
  puts Display.print("ERROR", "There was an error with the SQL query:\n\n#{msg.strip}")
  []
end

alias :run_query :run_sql
