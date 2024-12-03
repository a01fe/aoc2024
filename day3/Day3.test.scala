package aoc2024

import aoc2024.Day3.*

class TestDay3 extends munit.FunSuite:

  test("Check that Mul(x, y) multiplies"):
    assert(Mul(3, 4).result == 12)

  test("Read and parse part1 test data"):
    val p = os.pwd / "day3" / "test.txt"
    val r = List(
      Mul(2, 4),
      Mul(5, 5),
      Mul(11, 8),
      Mul(8, 5)
    )
    assert(parsePartOne(p).toList == r)

  test("Check part 1 results on test data"):
    val p = os.pwd / "day3" / "test.txt"
    assert(partOne(p) == 161)

  test("Read and parse part2 test data"):
    val p = os.pwd / "day3" / "test2.txt"
    val r = List(
      Mul(2, 4),
      DoNot(),
      Mul(5, 5),
      Mul(11, 8),
      Do(),
      Mul(8, 5)
    )
    assert(parsePartTwo(p).toList == r)

  test("Check evaluate with part2 test data"):
    val p = os.pwd / "day3" / "test2.txt"
    val is = parsePartTwo(p).toList
    assert(evaluate(is) == 48)
