require_relative 'common'
require 'sequel'
require "faker"
require 'active_support/core_ext/object/blank'
require 'active_support/core_ext/date/calculations'
require 'active_support/core_ext/time/calculations'
require 'hashie'
require_relative 'sql/csv_importer'
require_relative 'sql/charts'
require_relative 'sql/compare'

def clean_sql(sql)
  sql.gsub(/(\/\*([\s\S]*?)\*\/|--.*)/, "")
end

def split_sql_commands(sql)
  sql.split(/;[ \n\r]*$/).select(&:present?)
end

$sql = File.read('/home/codewarrior/solution.txt')
$sql_cleaned = clean_sql($sql)
$sql_commands = split_sql_commands($sql_cleaned)

# runs sql commands within a file. Useful for running external scripts such as importing data
def run_sql_file(file, &block)
  sql = clean_sql(File.read(file))

  split_sql_commands(sql).each do |cmd|
    result = cmd.downcase =~ /^(insert|create)/ ? DB.run(cmd) : DB[cmd]
    block.call(cmd, result) if block
  end
end

# the main method used when running user's code
def run_sql(limit: 100, cmds: $sql_commands, print: true, label: 'SQL Results', collapsed: false, &block)
  results = cmds.map do |cmd|
    dataset = (cmd.downcase =~ /^(insert|create)/ ? DB.run(cmd) : DB[cmd]) || []
    if dataset.count > 0

      lbl = label
      lbl += " (Top #{limit} of #{dataset.count})" if dataset.count > limit
      lbl = "-" + lbl if collapsed

      block.call(dataset, lbl) if block

      data = dataset.to_a.take(limit)
      Display.table(data, label: lbl) if print
      Display.prop("preview", true) if data.count > 10
    end
    dataset
  end

  results.select! {|r| r.count > 0 }

  if results.length > 1
    $sql_multi = true
    $sql_results = results
  else
    $sql_multi = false
    $sql_results = results.first || []
  end

rescue Sequel::DatabaseError => ex
  msg = ex.message.gsub("SQLite3::SQLException: ", "");
  puts Display.print("ERROR", "There was an error with the SQL query:\n\n#{msg.strip}")
  []
end

alias :run_query :run_sql

# useful helper for finding a specific record and wrapping it in a Hashie::Mash
def find_record(table, id)
  result = DB[table].where(id: id).first
  result ? Hashie::Mash.new(result) : nil
end

# helper method for returning the last queried dataset
def last_results
  $sql_multi ? $sql_results.last : $sql_results
end

# loops through each sql result and returns the row as a Hashie::Mash. If multiple results were returned,
# the last result set will be used
def each_result(results = last_results, &block)
  results.each do |result|
    block.call(Hashie::Mash.new(result))
  end
end

# returns the unique column values contained within the result set
def pluck_unique(column_name, results = last_results)
  results.map {|r| r[column_name]}.uniq
end

# connect the database

begin
  Display.status "Connecting to database..."
  eval(CONNECT_SQL)
rescue => ex
  if defined?(PG::ConnectionBad)
    if ex.is_a?(PG::ConnectionBad)
      sleep 1
      Display.status "Connection not ready, retrying in 1 second..."
      retry
    end
  end
end