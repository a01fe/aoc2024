package aoc2024.day1

class TestDay1 extends munit.FunSuite:

  test("Read and parse input data"):
    val (left, right) = Day1.read(os.pwd / "day1" / "test.txt")
    assert(left == Vector(1, 2, 3, 3, 3, 4))
    assert(right == Vector(3, 3, 3, 4, 5, 9))

  test("Check results on test data"):
    val (left, right) = Day1.read(os.pwd / "day1" / "test.txt")
    assert(Day1.part1(left, right) == 11)
    assert(Day1.part2(left, right) == 31)
