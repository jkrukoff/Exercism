defmodule Bob do
  @spec hey(String.t) :: String.t
  def hey(input) do
    is_question = String.ends_with?(String.trim_trailing(input), "?")
    is_empty = String.trim(input) == ""
    # Only consider the input to be uppercase if it's uppercase and if
    # there are characters that are effected by casing.
    is_uppercase = String.upcase(input) == input && String.downcase(input) != input

    cond do
      is_empty ->
        # Not saying anything.
        "Fine. Be that way!"
      is_question && is_uppercase ->
        # Yelling a question.
        "Calm down, I know what I'm doing!"
      is_uppercase ->
        # Yelling something other than a question.
        "Whoa, chill out!"
      is_question ->
        # Asking a question.
        "Sure."
      true ->
        # Everything else.
        "Whatever."
    end
  end
end
