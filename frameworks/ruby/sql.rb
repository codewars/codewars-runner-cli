require_relative 'common'
require 'sequel'
$sql = File.read('/home/codewarrior/solution.txt')

def run_sql
  DB[$sql]
end
