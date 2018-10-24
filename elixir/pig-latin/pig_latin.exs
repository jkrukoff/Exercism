defmodule PigLatin do
  @vowels ~W(a e i o u)
  @special_consonant ~W(x y)

  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.

  Words beginning with consonants should have the consonant moved to the end of
  the word, followed by "ay".

  Words beginning with vowels (aeiou) should have "ay" added to the end of the
  word.

  Some groups of letters are treated like consonants, including "ch", "qu",
  "squ", "th", "thr", and "sch".

  Some groups are treated like vowels, including "yt" and "xr".
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    String.split(phrase)
    |> Enum.map_join(" ", &translate_word/1)
  end

  defp translate_word(word) do
    {prefix, suffix} = split(word, [])
    suffix <> prefix <> "ay"
  end

  defp split("", acc) do
    # If we didn't find a vowel, return the whole string.
    {to_string(Enum.reverse(acc)), ""}
  end

  for vowel <- @vowels do
    defp split(<<unquote(vowel), phrase :: binary>>, acc) do
      # If we found a vowel, stop and return current split.
      {to_string(Enum.reverse(acc)), <<unquote(vowel), phrase :: binary>>}
    end
  end

  for vowel <- @vowels, consonant <- @special_consonant do
    defp split(<<unquote(consonant), unquote(vowel), phrase :: binary>>, acc) do
      # If we found a special consonant that isn't treated as a vowel,
      # split off the special consonant into the suffix and continue.
      split(<<unquote(vowel), phrase :: binary>>, [unquote(consonant) | acc])
    end
  end

  for consonant <- @special_consonant do
    defp split(<<unquote(consonant), phrase :: binary>>, acc) do
      # If we found a special consonant that is treated as a vowel,
      # stop and return the current split.
      {to_string(Enum.reverse(acc)), <<unquote(consonant), phrase :: binary>>}
    end
  end

  defp split(<<"qu", phrase :: binary>>, acc) do
    # Special case that is treated as a consonant, so split off into
    # suffix and continue.
    split(phrase, ["qu" | acc])
  end

  defp split(<<consonant :: utf8, phrase :: binary>>, acc) do
    # Consonant, split into suffix and continue.
    split(phrase, [<<consonant :: utf8>> | acc])
  end
end
