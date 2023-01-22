# Python script

from dataclasses import dataclass

class A:
    """Class A."""

@dataclass(frozen=True)
class Person:
    """A person."""

    name: str
    age: int

    def __str__(self) -> str:
        return f"{self.name}, age {self.age}"

def func_a():
    """A single-line docstring."""
    a = A()
    print(a)

def func_b():
    """
    A multiline docstring.

    This is an example of a multiline
    docstring created with ex-python.
    """
    pass

def main ():
    pass

if __name__ == "__main":
    main()
