require_relative 'common'
require 'sequel'
require "faker"
$sql = File.read('/home/codewarrior/solution.txt')

def run_sql(limit: 100, print: true)
  DB[$sql].to_a.tap do |results|
    Display.table(results.take(limit), label: 'Results') if print
    if results.count > limit
      puts "Truncated #{results.count} results to show only top 100"
    end
  end
rescue Sequel::DatabaseError => ex
  msg = ex.message.gsub("SQLite3::SQLException: ", "");
  puts Display.print("ERROR", "There was an error with the SQL query:\n\n#{msg.strip}")
  []
end

alias :run_query :run_sql
