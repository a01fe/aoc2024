package aoc2024.day3

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
    assert(Day3.parsePart1(p).toList == r)

  test("Check part 1 results on test data"):
    val p = os.pwd / "day3" / "test.txt"
    assert(Day3.part1(p) == 161)

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
    assert(Day3.parsePart2(p).toList == r)

  test("Check evaluate with part2 test data"):
    val p = os.pwd / "day3" / "test2.txt"
    val is = Day3.parsePart2(p)
    assert(Day3.evaluate(is) == 48)
