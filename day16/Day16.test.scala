package aoc2024.day16

import aoc2024.day16.Tile.*
import aoc2024.Point
import aoc2024.Location
import aoc2024.Direction.*

class TestDay16 extends munit.FunSuite:

  test("Verify Maze.toString() renders the maze"):
    val m = Vector(
      Vector(Wall,  Wall,  Wall,  Wall,  Wall),
      Vector(Wall,  Start, Empty, Empty, Wall),
      Vector(Wall,  Wall,  Empty, Wall,  Wall),
      Vector(Wall,  Empty, Empty, End,   Wall),
      Vector(Wall,  Wall,  Wall,  Wall,  Wall)
    )
    val s = """#####
              |#S..#
              |##.##
              |#..E#
              |#####
              |""".stripMargin
    assertEquals(Maze(m).toString(), s)

  test("Verify Day16.read reads and parses test data"):
    val o = Day16.read(os.pwd / "day16" / "test.txt")
    val s = """###############
              |#.......#....E#
              |#.#.###.#.###.#
              |#.....#.#...#.#
              |#.###.#####.#.#
              |#.#.#.......#.#
              |#.#.#####.###.#
              |#...........#.#
              |###.#.#####.#.#
              |#...#.....#.#.#
              |#.#.#.###.#.#.#
              |#.....#...#.#.#
              |#.###.#.#.#.#.#
              |#S..#.....#...#
              |###############
              |""".stripMargin
    assertEquals(o.toString(), s)

  test("Verify Maze.find returns location of a tile"):
    val m = Day16.read(os.pwd / "day16" / "test.txt")
    assertEquals(m.find(_ == Start), Point(1, 13))
    assertEquals(m.find(_ == End), Point(13, 1))

  test("Verify Maze.find fails if tile is not found"):
    val m = Maze(
      Vector(
        Vector(Wall,  Wall,  Wall,  Wall,  Wall),
        Vector(Wall,  Empty, Empty, Empty, Wall),
        Vector(Wall,  Wall,  Empty, Wall,  Wall),
        Vector(Wall,  Empty, Empty, End,   Wall),
        Vector(Wall,  Wall,  Wall,  Wall,  Wall)
      )
    )
    intercept[AssertionError](m.find(_ == Start))

  test("Verify Maze.find fails if more than one tile is found"):
    val m = Maze(
      Vector(
        Vector(Wall,  Wall,  Wall,  Wall,  Wall),
        Vector(Wall,  Start, Empty, Empty, Wall),
        Vector(Wall,  Wall,  Empty, Wall,  Wall),
        Vector(Wall,  Empty, Empty, Start, Wall),
        Vector(Wall,  Wall,  Wall,  Wall,  Wall)
      )
    )
    intercept[AssertionError](m.find(_ == Start))

  test("Verify nextSteps returns all possible next steps and their score"):
    val m = Maze(
      Vector(
        Vector(Wall,  Wall,  Wall,  Wall,  Wall),
        Vector(Wall,  Start, Empty, Empty, Wall),
        Vector(Wall,  Wall,  Empty, Wall,  Wall),
        Vector(Wall,  Empty, Empty, End,   Wall),
        Vector(Wall,  Wall,  Wall,  Wall,  Wall)
      )
    )
    assertEquals(
      m.nextSteps(Location(Point(1, 1), East)),
      List(
        (Location(Point(2, 1), East), 1)
      )
    )
    assertEquals(
      m.nextSteps(Location(Point(1, 2), East)),
      List(
        (Location(Point(2, 2), East), 1),
        (Location(Point(1, 1), North), 1000),
        (Location(Point(1, 3), South), 1000)
      )
    )
