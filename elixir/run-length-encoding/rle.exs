defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec encode(String.t()) :: String.t()
  def encode(string) do
    String.to_charlist(string)
    |> Enum.chunk_by(&(&1))
    |> Enum.map(&encode_run/1)
    |> to_string()
  end

  defp encode_run([character]) do
    character
  end

  defp encode_run([character | _] = string) do
    [to_string(length(string)), character]
  end

  @spec decode(String.t()) :: String.t()
  def decode(string) do
    String.to_charlist(string)
    |> tokenize()
    |> parse([])
    |> evaluate([])
    |> to_string()
  end

  defp tokenize(string) do
    Enum.chunk_by(string, &classify/1)
    |> Enum.map(&token/1)
  end

  defp classify(character) when character in '0123456789' do
    :number
  end

  defp classify(character) do
    {:character, character}
  end

  defp token([first | _] = string) do
    case classify(first) do
      :number ->
        {:number, string}
      {:character, _} ->
        {:character, string}
    end
  end

  defp parse([], acc) do
    Enum.reverse(acc)
  end

  defp parse([{:number, number}, {:character, character} | tokens], acc) do
    acc = [{:expand, List.to_integer(number), character} | acc]
    parse(tokens, acc)
  end

  defp parse([{:character, character} | tokens], acc) do
    acc = [{:literal, character} | acc]
    parse(tokens, acc)
  end

  defp evaluate([], acc) do
    Enum.reverse(acc)
  end

  defp evaluate([{:expand, number, character} | ops], acc) do
    acc = [List.duplicate(character, number) | acc]
    evaluate(ops, acc)
  end

  defp evaluate([{:literal, character} | ops], acc) do
    acc = [character | acc]
    evaluate(ops, acc)
  end
end
