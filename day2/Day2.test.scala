package aoc2024.day2

import Day2.*

class TestDay2 extends munit.FunSuite:

  test("Read and parse input data"):
    val rs = Day2.read(os.pwd / "day2" / "test.txt")
    assert(
      rs == List(
        List(7, 6, 4, 2, 1),
        List(1, 2, 7, 8, 9),
        List(9, 7, 6, 2, 1),
        List(1, 3, 2, 4, 5),
        List(8, 6, 4, 4, 1),
        List(1, 3, 6, 7, 9)
      )
    )

  test("Check isSafe on sample data"):
    assert(isSafe(List(7, 6, 4, 2, 1)) == true)
    assert(isSafe(List(1, 2, 7, 8, 9)) == false)
    assert(isSafe(List(9, 7, 6, 2, 1)) == false)
    assert(isSafe(List(1, 3, 2, 4, 5)) == false)
    assert(isSafe(List(8, 6, 4, 4, 1)) == false)
    assert(isSafe(List(1, 3, 6, 7, 9)) == true)
    assert(isSafe(List(11, 12, 15, 18, 19, 18)) == false)

  test("Check results on test data"):
    val rs = Day2.read(os.pwd / "day2" / "test.txt")
    assert(rs.count(isSafe(_)) == 2)
    assert(rs.count(isFuzzySafe(_)) == 4)
