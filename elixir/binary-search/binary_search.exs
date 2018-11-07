defmodule BinarySearch do
  @doc """
    Searches for a key in the tuple using the binary search algorithm.
    It returns :not_found if the key is not in the tuple.
    Otherwise returns {:ok, index}.

    ## Examples

      iex> BinarySearch.search({}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 5)
      {:ok, 2}

  """

  @spec search(tuple, integer) :: {:ok, integer} | :not_found
  def search({}, _key) do
    :not_found
  end

  def search(numbers, key) do
    partition(numbers, key, 0, tuple_size(numbers) - 1)
  end

  defguardp midpoint(first, last) when div(last - first, 2) + first

  defp partition(numbers, key, first, last) when elem(numbers, midpoint(first, last)) == key do
    {:ok, midpoint(first, last)}
  end

  defp partition(_numbers, _key, first, first) do
    :not_found
  end

  defp partition(numbers, key, first, last) when elem(numbers, midpoint(first, last)) < key do
    partition(numbers, key, midpoint(first, last) + 1, last)
  end

  defp partition(numbers, key, first, last) when elem(numbers, midpoint(first, last)) > key do
    partition(numbers, key, first, midpoint(first, last) - 1)
  end
end
