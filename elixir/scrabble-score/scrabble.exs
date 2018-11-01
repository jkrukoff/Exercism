defmodule Scrabble do
  @scores %{
    'AEIOULNRST' => 1,
    'DG' => 2,
    'BCMP' => 3,
    'FHVWY' => 4,
    'K' => 5,
    'JX' => 8,
    'QZ' => 10
  }

  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t()) :: non_neg_integer
  def score(word) do
    String.upcase(word)
    |> String.to_charlist()
    |> Enum.map(&letter_score/1)
    |> Enum.sum()
  end

  for {letters, score} <- Map.to_list(@scores) do
    defp letter_score(letter) when letter in unquote(letters) do
      unquote(score)
    end
  end

  defp letter_score(_) do
    0
  end
end
