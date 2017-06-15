defmodule CWFormatter do
  @moduledoc false

  @lf "<:LF:>"

  use GenEvent

  import ExUnit.Formatter, only: [format_time: 2, format_test_failure: 5]

  ## Callbacks

  def init(opts) do
    config = %{
      seed: opts[:seed],
      trace: opts[:trace],
      colors: Keyword.put_new(opts[:colors], :enabled, false),
      width: get_terminal_width(),
      tests_counter: 0,
      failures_counter: 0,
      skipped_counter: 0,
      invalids_counter: 0
    }
    {:ok, config}
  end

  def handle_event({:suite_started, _opts}, config) do
    {:ok, config}
  end

  def handle_event({:suite_finished, run_us, load_us}, _config) do
    IO.puts "\n<COMPLETEDIN::>" <> format_time(run_us, load_us)
    :remove_handler
  end

  def handle_event({:test_started, %ExUnit.Test{} = test}, config) do
    test_name = trace_test_name(test) |> nl_to_lf()
    IO.puts "\n<IT::>#{test_name}"
    {:ok, config}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: nil}}, config) do
    IO.puts "\n<PASSED::>Test Passed"
    {:ok, %{config | tests_counter: config.tests_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:skip, _}}}, config) do
    {:ok, %{config | tests_counter: config.tests_counter + 1,
                     skipped_counter: config.skipped_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:invalid, _}} = test}, config) do
    IO.puts "\n<ERROR::>" <> trace_test_result(test)

    {:ok, %{config | tests_counter: config.tests_counter + 1,
                     invalids_counter: config.invalids_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:failed, failed}} = test}, config) do
    IO.write "\n<FAILED::>" <> trace_test_result(test) <> @lf

    formatted = format_test_failure(test, failed, config.failures_counter + 1,
                                    config.width, fn _type, msg -> msg end)
    formatted |> nl_to_lf |> IO.puts

    {:ok, %{config | tests_counter: config.tests_counter + 1,
                     failures_counter: config.failures_counter + 1}}
  end

  def handle_event({:case_started, %ExUnit.TestCase{name: name}}, config) do
    IO.puts "\n<DESCRIBE::>" <> inspect(name)

    {:ok, config}
  end

  def handle_event({:case_finished, %ExUnit.TestCase{state: nil}}, config) do
    {:ok, config}
  end

  def handle_event({:case_finished, %ExUnit.TestCase{state: {:failed, _failed}}}, config) do
    {:ok, %{config | failures_counter: config.failures_counter + 1}}
  end

  ## Tracing

  defp trace_test_name(%ExUnit.Test{name: name}) do
    case Atom.to_string(name) do
      "test " <> rest -> rest
      rest -> rest
    end
  end

  defp trace_test_time(%ExUnit.Test{time: time}) do
    "#{format_us(time)}ms"
  end

  defp trace_test_result(test) do
    "#{trace_test_name test} (#{trace_test_time(test)})"
  end

  defp format_us(us) do
    us = div(us, 10)
    if us < 10 do
      "0.0#{us}"
    else
      us = div us, 10
      "#{div(us, 10)}.#{rem(us, 10)}"
    end
  end

  ## Printing

  defp nl_to_lf(string) when is_binary(string) do
    String.replace(string, "\n", @lf)
  end

  defp get_terminal_width do
    case :io.columns do
      {:ok, width} -> max(40, width)
      _ -> 80
    end
  end
end
