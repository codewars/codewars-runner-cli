class SqlCompare
  attr_reader :actual, :expected, :chart
  def initialize(expected, chart: nil, limit: 100, collapsed: false)
    @results = run_sql(label: '-Results: Actual', limit: limit, collapsed: collapsed)
    @actual = @results.to_a
    @expected = expected.to_a
    Display.table(expected.to_a.take(limit), label: 'Results: Expected', tab: true)

    @chart = Hashie::Mash.new(chart)
    draw_charts if @chart

  end

  def column(name, &block)
    @column_blocks ||= {}
    @column_blocks[name.to_sym] = block.to_proc
    self
  end

  def rows(&block)
    @rows_block = block.to_proc
    self
  end

  def spec(&block)
    _self = self
    column_blocks = @column_blocks || {}
    rows_block = @rows_block

    RSpec.describe "Query Tests" do
      let(:actual) { _self.actual }
      let(:expected) { _self.expected }

      expected.first.each do |key, value|
        describe "\"#{key}\" column" do
          it "should be included within results" do
            expect(actual.first).to have_key key
          end

          if value
            it "should be a #{value.class.name} value" do
              expect(actual.first[key]).to be_a value.class
            end
          end

          self.instance_eval(&column_blocks[key]) if column_blocks[key]
        end
      end

      describe :Rows do
        it "should have #{expected.count} rows" do
          expect(actual.count).to eq expected.count
        end

        self.instance_eval(&rows_block) if rows_block

        it "should should return the expected results" do
          expect(actual).to eq expected
        end
      end

      self.instance_eval(&block) if block
    end
  end

  protected

  def draw_charts
    case chart.type
      when :timeseries
        @actual_chart = time_series_chart(actual)
        @expected_chart = time_series_chart(expected)
    end
  end

  def time_series_chart(results)
    TimeSeriesChart.new(
    results,
    chart[:group_by],
    chart[:x],
    chart[:y],
    unit: chart.unit || 'day',
    sort: chart.sort != false
    ).tap do |chart|
      label = results == actual ? 'Chart: Actual' : 'Chart: Expected'
      chart.draw(label: label, type: 'TAB')
    end
  end
end