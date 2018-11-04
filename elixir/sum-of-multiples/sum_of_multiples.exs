defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    # Start with the smaller factors first, as they'll have more
    # multiples and our cost increases as we go.
    factors = Enum.sort(factors)

    # Build a list of factors already seen for each factor, so that we
    # can remove already seen multiples.
    {filter_on, _} =
      Enum.reduce(factors, {[[]], []}, fn factor, {all, acc} ->
        acc = [factor | acc]
        {[acc | all], acc}
      end)

    filter_on = Enum.reverse(filter_on)

    # Generate a stream of all valid multiples.
    Stream.flat_map(Enum.zip(factors, filter_on), fn {factor, filter_on} ->
      Stream.iterate(0, &(&1 + factor))
      |> Stream.take_while(&(&1 < limit))
      |> Stream.filter(fn candidate ->
        Enum.all?(filter_on, &(rem(candidate, &1) != 0))
      end)
    end)
    |> Enum.sum()
  end
end
