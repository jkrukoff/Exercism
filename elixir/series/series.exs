defmodule StringSeries do
  @doc """
  Given a string `s` and a positive integer `size`, return all substrings
  of that size. If `size` is greater than the length of `s`, or less than 1,
  return an empty list.
  """
  @spec slices(s :: String.t(), size :: integer) :: list(String.t())
  def slices(s, size) when size < 1 do
    []
  end

  def slices(s, size) do
    s_length = String.length(s)

    if size > s_length do
      []
    else
      for start <- 0..(s_length - size) do
        String.slice(s, start, size)
      end
    end
  end
end
