defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    Stream.map(factors, fn factor -> 
      Stream.iterate(0, &(&1 + factor))
      |> Stream.take_while(&(&1 < limit))
    end)
    |> Stream.concat()
    |> Stream.uniq()
    |> Enum.sum()
  end
end
