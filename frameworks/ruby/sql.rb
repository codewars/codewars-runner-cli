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

# Silence "SEQUEL DEPRECATION WARNING:"
# <http://sequel.jeremyevans.net/rdoc/classes/Sequel/Deprecation.html>
Sequel::Deprecation::output = false

def clean_sql(sql)
  sql.gsub(/(\/\*([\s\S]*?)\*\/|--.*)/, "")
end

def select_cmd?(cmd)
  (cmd.strip =~ /^(SELECT|WITH)/i) == 0
end

def run_cmd(cmd)
  try_connection do
    select_cmd?(cmd) ? DB[cmd] : DB.run(cmd)
  end
end

def split_sql_commands(sql)
  # first we want to seperate select statements into chunks
  chunks = sql.split(/;[ \n\r]*$/i).select(&:present?).chunk { |l| select_cmd?(l) }
  # select statements need to stay individual so that we can return individual datasets, but we can group other statements together
  [].tap do |final|
    chunks.each do |select, cmds|
      if select
        final.concat(cmds)
      else
        final << cmds.join(";\n")
      end
    end
  end
end

def read_file(file, delete = false)
  if File.exists?(file)
    File.read(file).tap do
       `rm -rf #{file}` if delete
    end
  end
end

$sql = read_file('/workspace/solution.txt') || read_file('/workspace/solution.sql')
if $sql
    $sql_cleaned = clean_sql($sql)
    $sql_commands = split_sql_commands($sql_cleaned)
end

# runs sql commands within a file. Useful for running external scripts such as importing data
def run_sql_file(file, &block)
  sql = clean_sql(File.read(file))

  split_sql_commands(sql).map do |cmd|
    run_cmd(cmd).tap do |result|
      block.call(cmd, result) if block
    end
  end
end

# the main method used when running user's code
def run_sql(limit: 100, cmds: $sql_commands, print: true, label: 'SELECT Results', collapsed: false, &block)
  Display.status("Running sql commands...")
  cmds = [cmds] if cmds.is_a? String
  results = cmds.map do |cmd|
    dataset = run_cmd(cmd) || []
    if dataset.count > 0

      lbl = label
      lbl += " (Top #{limit} of #{dataset.count})" if dataset.count > limit
      lbl = "-" + lbl if collapsed

      block.call(dataset, lbl) if block

      Display.table(dataset.to_a.take(limit), label: lbl, allow_preview: true) if print
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

def try_connection
  begin
    yield
  rescue Sequel::DatabaseConnectionError => cex
    Display.status "Connection error: #{cex.message}, retrying in 1 second..."
    sleep 1
    retry
  rescue => ex
    if defined?(PG::ConnectionBad)
      if ex.is_a?(PG::ConnectionBad)
        sleep 1
        Display.status "Connection not ready, retrying in 1 second..."
        retry
      end
    end
  end
end

# connect the database
try_connection do
  Display.status "Connecting to database..."
  # Setup database connection
  DATABASE = ENV['DATABASE_NAME']

  case ENV['DATABASE_TYPE']
    when 'sqlite'
      DB = Sequel.sqlite

    when 'postgres'
      DB = Sequel.connect("postgres://localhost/#{DATABASE}")
  end

  expected_file = read_file('/workspace/expected.sql', true)
  if expected_file
    eval <<-END
      def expected
        DB[%q(#{expected_file})].to_a
      end
    END
  end
end
