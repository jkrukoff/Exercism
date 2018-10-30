def is_leap_year(year: int) -> bool:
    """
    Determine if the given year is a leap year.

    year -- An integer year.
    returns -- The leap year status as true/false.
    """

    return year % 400 == 0 or (year % 100 != 0 and year % 4 == 0)
