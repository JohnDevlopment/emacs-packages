# Python script

from __future__ import annotations
from dataclasses import dataclass

class SomeDescriptor:
    def __get__(self, obj, objtype=None):
        pass

    def __set__(self, obj, value) -> None:
        pass

class A:
    """Class A."""

    def __getitem__(self, key, /):
        pass

    def __setitem__(self, key, value, /) -> None:
        pass

    def __getattr__(self, key: str):
        pass

    def __setattr__(self, key: str, value) -> None:
        pass

    def __len__(self) -> int:
        return 0

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

if __name__ == "__main__":
    main()
