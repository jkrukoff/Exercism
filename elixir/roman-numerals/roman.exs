defmodule Roman do
  @values [
    {1000, "M"},
    {900, "CM"},
    {500, "D"},
    {400, "CD"},
    {100, "C"},
    {90, "XC"},
    {50, "L"},
    {40, "XL"},
    {10, "X"},
    {9, "IX"},
    {5, "V"},
    {4, "IV"},
    {1, "I"},
  ]

  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) do
    numerals(number, @values, [])
  end

  defp numerals(0, _values, acc) do
    to_string(Enum.reverse(acc))
  end

  defp numerals(number, [{value, digit} | next_value] = values, acc) do
    if number - value < 0 do
      numerals(number, next_value, acc)
    else
      numerals(number - value, values, [digit | acc])
    end
  end
end
