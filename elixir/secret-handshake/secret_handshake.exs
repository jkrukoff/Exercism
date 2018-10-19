defmodule SecretHandshake do
  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump
  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) do
    <<reverse :: size(1),
      jump :: size(1),
      close :: size(1),
      blink :: size(1),
      wink :: size(1)>> = <<code :: size(5)>>

    # Check flags in reverse priority, since we build the list in
    # reverse order.
    actions = add_if([], jump, "jump")
              |> add_if(close, "close your eyes")
              |> add_if(blink, "double blink")
              |> add_if(wink, "wink")

    if reverse == 1 do
      Enum.reverse(actions)
    else
      actions
    end
  end

  defp add_if(actions, 0, action) do
    actions
  end

  defp add_if(actions, 1, action) do
    [action | actions]
  end
end
