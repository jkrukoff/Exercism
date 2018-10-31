import string


def is_pangram(sentence: str) -> bool:
    """
    Determine if a given string contains all the characters from a to z.

    sentence -- Any string.
    returns -- true/false for if string contains all letters from a to z.
    """

    letters = set(string.ascii_lowercase)
    return letters.issubset(sentence.lower())
