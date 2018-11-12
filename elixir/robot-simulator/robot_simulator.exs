defmodule RobotSimulator do
  @directions ~W(north east south west)a

  defstruct [:direction, :position]

  @type error :: {:error, String.t()}
  @type direction :: :north | :east | :south | :west
  @type position :: {integer, integer}
  @type instructions :: String.t()
  @type robot :: %RobotSimulator{direction: direction, position: position}

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: any, position :: any) :: robot | error
  def create(direction \\ :north, position \\ {0, 0})

  def create(direction, _position) when direction not in @directions do
    {:error, "invalid direction"}
  end

  def create(direction, {x, y}) when is_integer(x) and is_integer(y) do
    %RobotSimulator{direction: direction, position: {x, y}}
  end

  def create(_direction, _position) do
    {:error, "invalid position"}
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: robot, instructions :: instructions) :: robot | error
  def simulate(robot, instructions) do
    Enum.reduce(to_charlist(instructions), robot, &instruction/2)
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: robot) :: direction
  def direction(%RobotSimulator{direction: direction}) do
    direction
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: robot) :: position
  def position(%RobotSimulator{position: position}) do
    position
  end

  defp instruction(_, {:error, _} = error) do
    error
  end

  for {direction, turn} <- Enum.zip(@directions, ~W(west north east south)a) do
    defp instruction(?L, %RobotSimulator{direction: unquote(direction)} = robot) do
      %RobotSimulator{robot | direction: unquote(turn)}
    end
  end

  for {direction, turn} <- Enum.zip(@directions, ~W(east south west north)a) do
    defp instruction(?R, %RobotSimulator{direction: unquote(direction)} = robot) do
      %RobotSimulator{robot | direction: unquote(turn)}
    end
  end

  defp instruction(?A, %RobotSimulator{position: {x, y}, direction: :north} = robot) do
    %RobotSimulator{robot | position: {x, y + 1}}
  end

  defp instruction(?A, %RobotSimulator{position: {x, y}, direction: :east} = robot) do
    %RobotSimulator{robot | position: {x + 1, y}}
  end

  defp instruction(?A, %RobotSimulator{position: {x, y}, direction: :south} = robot) do
    %RobotSimulator{robot | position: {x, y - 1}}
  end

  defp instruction(?A, %RobotSimulator{position: {x, y}, direction: :west} = robot) do
    %RobotSimulator{robot | position: {x - 1, y}}
  end

  defp instruction(_instruction, _robot) do
    {:error, "invalid instruction"}
  end
end
