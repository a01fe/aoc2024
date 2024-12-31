package aoc2024.day10

import aoc2024.Point

class TestDay10 extends munit.FunSuite:

  val tm = TrailMap(
    Vector(
      Vector(8, 9, 0, 1, 0, 1, 2, 3),
      Vector(7, 8, 1, 2, 1, 8, 7, 4),
      Vector(8, 7, 4, 3, 0, 9, 6, 5),
      Vector(9, 6, 5, 4, 9, 8, 7, 4),
      Vector(4, 5, 6, 7, 8, 9, 0, 3),
      Vector(3, 2, 0, 1, 9, 0, 1, 2),
      Vector(0, 1, 3, 2, 9, 8, 0, 1),
      Vector(1, 0, 4, 5, 6, 7, 3, 2)
    )
  )

  val tm1 = TrailMap(
    Vector(
      Vector(0, 1, 2, 3),
      Vector(1, 2, 3, 4),
      Vector(8, 7, 6, 5),
      Vector(9, 8, 7, 6)
    )
  )

  val tm3 = TrailMap(
    Vector(
      Vector(-2, -2, 9, 0, -2, -2, 9),
      Vector(-2, -2, -2, 1, -2, 9, 8),
      Vector(-2, -2, -2, 2, -2, -2, 7),
      Vector(6, 5, 4, 3, 4, 5, 6),
      Vector(7, 6, 5, -2, 9, 8, 7),
      Vector(8, 7, 6, -2, -2, -2, -2),
      Vector(9, 8, 7, -2, -2, -2, -2)
    )
  )

  test("Day10.read() reads and parses TrailMap"):
    assertEquals(Day10.read(os.pwd / "day10" / "test1.txt"), tm1)
    assertEquals(Day10.read(os.pwd / "day10" / "test3.txt"), tm3)
    assertEquals(Day10.read(os.pwd / "day10" / "test.txt"), tm)

  test("TrailMap.trailheads returns list of all trailheads"):
    assertEquals(tm1.trailheads, List(Point(0, 0)))
    assertEquals(tm3.trailheads, List(Point(3, 0)))
    val th = List(
      Point(2,0),
      Point(4,0),
      Point(4,2),
      Point(6,4),
      Point(2,5),
      Point(5,5),
      Point(0,6),
      Point(6,6),
      Point(1,7)
    )
    assertEquals(tm.trailheads, th)

  test("TrailMap.stepsTo returns list of valid steps to next points"):
    assertEquals(tm.stepsTo(1, Point(2, 0)).toSet, List(Point(3, 0), Point(2, 1)).toSet)

  test("TrailMap.stepsTo returns empty list when no step are available"):
    assertEquals(tm.stepsTo(2, Point(2, 0)), List.empty[Point])
