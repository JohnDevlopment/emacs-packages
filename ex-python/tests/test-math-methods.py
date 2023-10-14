# Python script

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal, getcontext, FloatOperation
from typing_extensions import Self
from typing import Callable
import sys, logging, re

class Money:
    """Class representing a monetary amount."""

    def __init__(self, cents: int | float, /):
        self.amount_cents = int(cents)

    @classmethod
    def from_string(cls, value: str, /) -> Self:
        """Return an object representation of VALUE."""
        if (m := re.fullmatch(r'([0-9]+)\.([0-9]{1,2})', value)):
            # Decimal number: one or more digits left of point,
            # one or two right of point
            dollars, cents = m.group(1, 2)
            if len(cents) == 1:
                cents += "0"

            return cls(int(dollars) * 100 + int(cents))

        m = re.fullmatch(r'[0-9]+', value)
        if not m:
            raise ValueError(f"Invalid value '{value}', must be integer or float value (x.yy format)")

        return cls(int(value))

    def as_tuple(self) -> tuple[int, int]:
        """Return monetary value as tuple."""
        dollars = 0
        cents = self.amount_cents

        while cents >= 100:
            dollars += 1
            cents -= 100

        return (dollars, cents)

    def __str__(self) -> str:
        dollars, cents = self.as_tuple()
        return f"${dollars}.{cents:02}"

    def __add__(self, other: Self) -> Self:
        return Money(self.amount_cents + other.amount_cents)

    def __sub__(self, other: Self):
        return Money(self.amount_cents - other.amount_cents)

    def __mul__(self, other: float | int) -> Self:
        return Money(int(self.amount_cents * other))

    def __truediv__(self, other: float | int) -> Self:
        return Money(int(self.amount_cents / other))

    # Ignored
    #

    # def __matmul__(self, other):
    #     return self @ other

    # def __floordiv__(self, other):
    #     return self // other

    # def __mod__(self, other):
    #     return self % other

    # def __pow__(self, other):
    #     return self ** other

    # def __lshift__(self, other):
    #     return self << other

    # def __rshift__(self, other):
    #     return self >> other

    # def __and__(self, other):
    #     return self & other

    # def __xor__(self, other):
    #     return self ^ other

    # def __or__(self, other):
    #     return self | other

    # def __radd__(self, other):
    #     return self + other

    # def __rsub__(self, other):
    #     return self - other

    # def __rmul__(self, other):
    #     return self * other

    # def __rmatmul__(self, other):
    #     return self @ other

    # def __rtruediv__(self, other):
    #     return self / other

    # def __rfloordiv__(self, other):
    #     return self // other

    # def __rmod__(self, other):
    #     return self % other

    # def __rpow__(self, other):
    #     return self ** other

    # def __rlshift__(self, other):
    #     return self << other

    # def __rrshift__(self, other):
    #     return self >> other

    # def __rand__(self, other):
    #     return self & other

    # def __rxor__(self, other):
    #     return self ^ other

    # def __ror__(self, other):
    #     return self | other

def testfunc_init() -> None:
    value = input("Your balance (e.g., 1.5): ")
    balance = Money.from_string(value)
    print(balance)

def testfunc_add() -> None:
    value_a = Money.from_string(input("A: "))
    value_b = Money.from_string(input("B: "))
    print(f"{value_a} + {value_b} = {value_a + value_b}")

def testfunc_sub() -> None:
    value_a = Money.from_string(input("A: "))
    value_b = Money.from_string(input("B: "))
    print(f"{value_a} - {value_b} = {value_a - value_b}")

def testfunc_mul() -> None:
    value_a = Money.from_string(input("A: "))
    value_b = float(input("B: "))
    print(f"{value_a} * {value_b} = {value_a * value_b}")

def testfunc_div() -> None:
    value_a = Money.from_string(input("A: "))
    value_b = float(input("B: "))
    print(f"{value_a} / {value_b} = {value_a / value_b}")

def get_test_funcs() -> dict[str, Callable[[], None]]:
    gd = globals()
    res = {}

    for k, v in gd.items():
        m = re.match(r'testfunc_(.+)', k)
        if m:
            res[m[1]] = v

    return res

def main():
    logging.basicConfig(level=logging.DEBUG)

    FUNCTIONS = get_test_funcs()
    assert len(FUNCTIONS) != 0

    try:
        func_name = sys.argv[1]
        func = FUNCTIONS[func_name]
        return func()
    except IndexError:
        logging.error("No argument")
        logging.info("Valid function names are: %s", ", ".join(FUNCTIONS.keys()))
        return 1
    except KeyError as exc:
        logging.error("Unknown function name: %r", exc)
        logging.info("Valid function names are: %s", ", ".join(FUNCTIONS.keys()))
        return 1

    return 0

if __name__ == "__main__":
    main()
