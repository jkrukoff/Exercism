defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    match_first = ~r/[[:upper:]]|\b[[:alpha:]]/u
    Regex.scan(match_first, string)
    |> to_string()
    |> String.upcase()
  end
end
