defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    String.to_charlist(text)
    |> Enum.map(&(rotate_character(&1, shift)))
    |> List.to_string()
  end

  defp rotate_character(character, shift) when character in ?a..?z do
    rotate_character(character, shift, ?a)
  end

  defp rotate_character(character, shift) when character in ?A..?Z do
    rotate_character(character, shift, ?A)
  end

  defp rotate_character(character, shift) do
    character
  end

  defp rotate_character(character, shift, from) do
    rem(character - from + shift, 26) + from
  end
end
