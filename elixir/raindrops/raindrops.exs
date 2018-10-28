defmodule Raindrops do
  @factors [3, 5, 7]

  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
    just pass the number's digits straight through.
  """
  @spec convert(pos_integer) :: String.t()
  def convert(number) do
    raindrops = for f <- @factors, rem(number, f) == 0 do
      sound(f)
    end

    if raindrops == [] do
        number
    else
        raindrops
    end
    |> to_string()
  end

  defp sound(3), do: "Pling"
  defp sound(5), do: "Plang"
  defp sound(7), do: "Plong"
end
