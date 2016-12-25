require 'csv'
require 'chronic'

# data importer utility
class CsvImporter
  attr_reader :fields, :csv, :table, :limit, :random

  def initialize(file, table, fields: {}, limit: 500, random: false)
    @csv = CSV.read(file)
    @table = table
    @fields = fields
    @limit = limit
    @random = random
  end

  def create_schema
    importer = self
    DB.create_table @table do
      importer.csv.first.each do |field|
        if importer.fields[field]
          column field, importer.fields[field]
        else
          String field
        end
      end
    end
  end

  def convert_value(field, value)
    case @fields[field]
      when DateTime
        Chronic.parse(value)
      when Integer
        value.gsub(',', '').to_i
      else
        value
    end
  end

  def import(skip_schema: false)
    create_schema unless skip_schema

    Display.status("Importing #{[limit, @csv.count].min} records...")

    fields = @csv.first
    dataset = DB[@table]

    # remove the columns row
    rows = @csv.to_a.drop(1)
    # randomize the rows if random is enabled
    rows = rows.to_a.sample(limit) if random

    rows.each.with_index do |line, row|
      return if row > limit

      data = {}

      line.each.with_index do |value, col|
        data[fields[col]] = convert_value(fields[col], value)
      end

      dataset.insert(data)
    end
  end

  def self.import_sales_data(random: false, limit: 300)
    importer = CsvImporter.new("/runner/sample_data/sales.csv", :sales, random: random, limit: limit, fields: {
      'latitude' => Float,
      'longitude' => Float,
      'price' => Integer
    })

    # TODO: figure out how to fix datetime fields for SQLite
    unless defined?(Sequel::SQLite)
      importer.fields.merge(
        'transation_date' => DateTime,
        'account_created' => DateTime,
        'last_login' => DateTime
      )
    end
    importer.import
  end
end
