#!/usr/bin/env python3


def nth(n, init):
    m = max(n, *init) + 1
    lastSeen = [0] * m  # Big enough
    for i, v in enumerate(init[:-1]):
        lastSeen[v] = i + 1
    nextToSay = init[-1]
    for turn in range(len(init), n):
        l = lastSeen[nextToSay]
        if l == 0:
            lastSeen[nextToSay] = turn
            nextToSay = 0
        else:
            lastSeen[nextToSay] = turn
            nextToSay = turn - l
    return nextToSay

"""
import pytest


def test_nth():
    assert nth(4, [0, 3, 6]) == 0
    assert nth(5, [0, 3, 6]) == 3
    assert nth(6, [0, 3, 6]) == 3
    assert nth(7, [0, 3, 6]) == 1
    assert nth(8, [0, 3, 6]) == 0
    assert nth(9, [0, 3, 6]) == 4
    assert nth(10, [0, 3, 6]) == 0


def test_big():
    assert nth(2020, [0,3,6]) == 436


@pytest.mark.parametrize("l,v", [
        ([0, 3, 6], 175594),
        ([1, 3, 2], 2578),
        ([2, 1, 3], 3544142),
        ([1, 2, 3], 261214),
        ([2, 3, 1], 6895259),
        ([3, 2, 1], 18),
        ([3, 1, 2], 362),
])
def test_huge(l, v):
    assert nth(30000000, l) == v
"""


if __name__ == '__main__':
    print(nth(30000000, [1, 0, 16, 5, 17, 4]))
