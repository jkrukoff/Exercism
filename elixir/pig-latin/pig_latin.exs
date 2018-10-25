defmodule PigLatin do
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

  # Now I've got two problems.
  @match_vowel ~r/
    # A q not followed by a u.
    q(?'not_qu'[aeio]) |
    # A not q followed by a vowel.
    (^|[^q])(?'vowel'[aeiou]) |
    # A special consonant followed by a consonant.
    (?'special'[xy][^aeiou])
  /iux
  defp translate_word(word) do
    case Regex.split(@match_vowel, word, parts: 2, on: [:not_qu, :vowel, :special], include_captures: true) do
        [prefix, match, suffix] ->
          match <> suffix <> prefix <> "ay"
        [suffix] ->
          suffix <> "ay"
    end
  end
end
