defmodule SpaceAge do
  @type planet ::
          :mercury
          | :venus
          | :earth
          | :mars
          | :jupiter
          | :saturn
          | :uranus
          | :neptune

  @seconds_in_year 31557600

  @conversions [
    {:mercury, 0.2408467},
    {:venus, 0.61519726},
    {:earth, 1.0},
    {:mars, 1.8808158},
    {:jupiter, 11.862615},
    {:saturn, 29.447498},
    {:uranus, 84.016846},
    {:neptune, 164.79132},
  ]

  @doc """
  Return the number of years a person that has lived for 'seconds' seconds is
  aged on 'planet'.
  """
  @spec age_on(planet, pos_integer) :: float

  for {planet, conversion} <- @conversions do
    def age_on(unquote(planet), seconds) do
      seconds / (@seconds_in_year * unquote(conversion))
    end
  end
end
