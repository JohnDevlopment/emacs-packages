# Python script

from __future__ import annotations
from dataclasses import dataclass
import sys, logging

class SomeDescriptor:
    def __get__(self, obj, objtype=None):
        return getattr(obj, self.private_name)

    def __set__(self, obj, value) -> None:
        setattr(obj, self.private_name, value)

    def __set_name__(self, owner, name: str) -> None:
        self.owner = owner
        self.name = name
        self.private_name = f"_{name}"

class A:
    """Class A."""

    name = SomeDescriptor()

    def __init__(self, name: str):
        self.name = name

    def __str__(self) -> str:
        return "A: " + self.name

@dataclass(frozen=True)
class Person:
    """A person."""

    name: str
    age: int

    def __str__(self) -> str:
        return f"{self.name}, age {self.age}"

    # Comparison methods #

    def __eq__(self, other) -> bool:
        ...

    def __ne__(self, other) -> bool:
        ...

    def __lt__(self, other) -> bool:
        ...

    def __le__(self, other) -> bool:
        ...

    def __gt__(self, other) -> bool:
        ...

    def __ge__(self, other) -> bool:
        ...

    ###

def func_a():
    """A single-line docstring."""
    a1, a2, a3 = A("John"), A("Bob"), A("Crunk")
    print(a1, a2, a3)

def func_b():
    """
    A multiline docstring.

    This is an example of a multiline
    docstring created with ex-python.
    """
    pass

def main() -> int:
    logging.basicConfig(level=logging.INFO)

    try:
        func_name = sys.argv[1]
        func = globals()[func_name]
        return func()
    except IndexError:
        logging.error("No argument")
        return 1
    except KeyError as exc:
        logging.error("Unknown function name: %r", exc)
        return 1

    return 0

if __name__ == "__main__":
    sys.exit(main())
