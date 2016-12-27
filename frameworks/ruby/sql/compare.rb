def compare_with(expected, limit: 100, collapsed: false, &block)
  sql_compare = SqlCompare.new(expected, limit: limit, collapsed: collapsed)
  sql_compare.instance_eval(&block) if block

  sql_compare.spec
  sql_compare.actual
end

class SqlCompare
  attr_reader :actual, :expected, :chart

  def initialize(expected, chart: nil, limit: 100, collapsed: false)
    @results = run_sql(label: 'Results: Actual', limit: limit, collapsed: collapsed)
    @actual = ($sql_multi ? @results.last.to_a : @results.to_a)
    @limit = limit
    Display.status("Running expected query...")
    @expected = expected.to_a

    Display.log('No rows returned', 'Results: Actual') if @actual.size == 0
    Display.table(expected.to_a, label: 'Results: Expected', tab: true, allow_preview: true)

    draw_chart(chart) if chart

    @column_blocks ||= {}
    @rows_blocks = []
  end

  def draw_chart(config)
    @chart = Hashie::Mash.new(config)
    draw_charts
  end

  def column(name, &block)
    @column_blocks[name.to_sym] = block.to_proc
    self
  end

  def rows(&block)
    @rows_blocks << block.to_proc
    self
  end


  def spec(&block)
    return if @spec_called
    @spec_called = true

    _self = self
    column_blocks = @column_blocks
    rows_blocks = @rows_blocks
    it_blocks = @it_blocks

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

        rows_blocks.each do |block|
          self.instance_eval(&block)
        end

        it "should should return the expected results" do
          limit = 10
          expect(actual.take(limit)).to eq expected.take(limit)
          expect(actual == expected).to be true
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
    label = results == actual ? 'Chart: Actual' : 'Chart: Expected'
    TimeSeriesChart.new(
      results,
      chart[:group_by],
      chart[:x],
      chart[:y],
      unit: chart.unit || 'day',
      sort: chart.sort != false
    ).tap do |chart|
      chart.draw(label: label, type: 'TAB')
    end
  rescue => ex
    Display.print('TAB', "Failed to render chart: #{ex.message}", label: label)
  end
end