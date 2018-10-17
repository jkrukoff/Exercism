defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map
  def count(sentence) do
    String.downcase(sentence)
    |> String.split(~r/[^[:alnum:]-]+/u, trim: true)
    |> Enum.reduce(
      %{},
      fn word, counts ->
        Map.update(counts, word, 1, &(&1 + 1))
      end
    )
  end
end
