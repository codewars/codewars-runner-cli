require 'json'

# helper class used to render ChartJS results
class Chart
  attr_reader :results

  def initialize(results, type)
    @results = results
    @type = type
  end

  def options
    {}
  end

  def data
    {}
  end

  def json
    @json ||= { type: @type, data: data, options: options }
    @json.to_json
  end

  def draw(type: "LOG", label: "Chart")
    Display.print(type, json, mode: "CHART", label: label)
  end

  def colors
    @colors ||= [
      "#79bb1f",
      "#358bd6",
      "#c6081e",
      "#dddd4b",
      "#a081db",
      "#23ceb1",
      "#b48c08",
      "#4bce23"
    ]
  end

  def hex_to_rgba(hex, alpha = 0.2)
    "rgba(#{hex.gsub("#","").scan(/.{2}/).map {|h| h.to_i(16) }.join(", ")}, #{alpha})"
  end

  def next_color
    color = colors.shift || Faker::Color.hex_color
    @type == 'bar' ? color : hex_to_rgba(color)
  end
end

class TimeSeriesChart < Chart
  attr_reader :group_by, :x_column, :y_column, :unit

  # == parameters:
  # group_by::
  #   A Symbol representing the field that should be grouped
  # x::
  #   A Symbol representing the DateTime field that will be used as the x axes
  # y::
  #   A Symbol representing the number field that will be aggregated (y axes)
  # unit::
  #   A String of either (day, month, year) that indicates how the dates are grouped (x axes)
  # type::
  #   A string indicating the chart type. Should either be "line" or "bar"
  # sort::
  #   A boolean indicating if date sorting should be used. If true (default), it will sort the dates from oldest to newest
  def initialize(results, group_by, x, y, unit: 'day', type: 'bar', sort: true)
    super(results, type)
    @group_by = group_by
    @x_column = x
    @y_column = y
    @unit = unit
    @sort = sort
    # puts "<OUT:SCRIPT:>//cdnjs.cloudflare.com/ajax/libs/moment.js/2.13.0/moment.min.js"
  end

  def format(date)
    case unit
      when 'day' then date.strftime("%-m/%-d")
      when 'month' then date.strftime("%b %-y")
      when 'year' then date.strftime("%Y")
    end
  end

  def data
    @data ||= {
    datasets: datasets_data.map do |label, data|
      { label: label, data: y_data(data), backgroundColor: next_color }
    end,
    labels: sorted_labels
    }
  end

  def y_data(data)
    sorted_labels.map do |label|
      data[label]
    end
  end

  def labels
    @labels ||= {}
  end

  def sorted_labels
    @sort ? @sorted_labels ||= labels.sort_by{|k, v| v}.map {|k,v| k } : labels.keys
  end

  def datasets_data
    @datasets ||= {}.tap do |ds|
      results.each do |row|
        x = format(row[x_column])
        # automatically group/sum by timeframe. Useful if a group by was not actually provided in the query
        ds[row[group_by]] ||= {}
        ds[row[group_by]][x] ||= 0
        ds[row[group_by]][x] += row[y_column] || 0
        labels[x] = row[x_column]
      end
    end
  end
end