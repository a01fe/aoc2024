package aoc2024.day14

import aoc2024.Point

class TestDay10 extends munit.FunSuite:

  test("Verify Day14.read can read and parse a Room"):
    val r = Room(
      Seq(
        Robot(Point(0, 4), Point(3, -3)),
        Robot(Point(6, 3), Point(-1, -3)),
        Robot(Point(10, 3), Point(-1, 2)),
        Robot(Point(2, 0), Point(2, -1)),
        Robot(Point(0, 0), Point(1, 3)),
        Robot(Point(3, 0), Point(-2, -2)),
        Robot(Point(7, 6), Point(-1, -3)),
        Robot(Point(3, 0), Point(-1, -2)),
        Robot(Point(9, 3), Point(2, 3)),
        Robot(Point(7, 3), Point(-1, 2)),
        Robot(Point(2, 4), Point(2, -3)),
        Robot(Point(9, 5), Point(-3, -3))
      ),
      Point(11, 7)
    )
    assertEquals(Day14.read(os.pwd / "day14" / "test.txt"), r)

  test("Verify Robot.pAt returns position at specified time"):
    val room = Room(Seq(Robot(Point(2, 4), Point(2, -3))), Point(11, 7))
    val robot = room.rs.head
    given Room = room
    assertEquals(robot.pAt(0), Point(2, 4))
    assertEquals(robot.pAt(1), Point(4, 1))
    assertEquals(robot.pAt(2), Point(6, 5))
    assertEquals(robot.pAt(3), Point(8, 2))
    assertEquals(robot.pAt(4), Point(10, 6))
    assertEquals(robot.pAt(5), Point(1, 3))

  test("Verify safetyFactor returns correct value for example data"):
    val room = Day14.read(os.pwd / "day14" / "test.txt")
    assertEquals(room.safetyFactorAt(100), 12)
