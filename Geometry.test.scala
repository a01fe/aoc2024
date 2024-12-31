package aoc2024

class TestGeometry extends munit.FunSuite:

  test("Test Point =="):
    assertEquals(Point(0, 0) == Point(0, 0), true)
    assertEquals(Point(0, 1) == Point(0, 0), false)

  test("Test Direction.delta"):
    assertEquals(Direction.North.delta, Point(0, -1))
    assertEquals(Direction.West.delta, Point(-1, 0))
    assertEquals(Direction.South.delta, Point(0, 1))
    assertEquals(Direction.East.delta, Point(1, 0))

  test("Test Direction.turnRight"):
    assertEquals(Direction.North.turnLeft, Direction.West)
    assertEquals(Direction.West.turnLeft, Direction.South)
    assertEquals(Direction.South.turnLeft, Direction.East)
    assertEquals(Direction.East.turnLeft, Direction.North)
