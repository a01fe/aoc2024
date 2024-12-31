package aoc2024.day6

class TestDay6 extends munit.FunSuite:

  test("verify part1 returns correct result for test data"):
    val r = Day6.read(os.pwd / "day6" / "test.txt")
    assertEquals(r.part1, 41)
