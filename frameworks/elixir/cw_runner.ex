defmodule CWRunner do
  @moduledoc false

  def run(solution_path, fixture_path) do
    with :ok <- safe_compile(solution_path, "solution"),
         :ok <- init_ex_unit,
         :ok <- safe_compile(fixture_path, "fixture")
    do
      :ok
    end
    # This will look much prettier in Elixir 1.3
    |> case do
      :ok -> :ok
      {:error, message} ->
        IO.puts "\n<ERROR::>" <> message
    end
  end

  def init_ex_unit do
    Code.load_file("frameworks/elixir/cw_formatter.ex")
    ExUnit.start([seed: 0, formatters: [CWFormatter]])
    :ok
  end

  def safe_compile(path, subject) do
    try do
      code = File.read!(path)
      # Use Code.compile_string to have simple filename
      Code.compile_string(code, subject)
      :ok
    rescue
      error -> {:error, Exception.message(error)}
    end
  end
end
