package aoc2024.day15

import aoc2024.day15.Cell.*
import aoc2024.day15.Move.{Left,Right,Up,Down}
import scala.collection.mutable.ArraySeq
import aoc2024.Point

class TestDay15 extends munit.FunSuite:

  val simulation = FunFixture[Simulation](
    setup = t => Simulation(
      ArraySeq(
        ArraySeq(Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall),
        ArraySeq(Wall,  Empty, Empty, Box,   Empty, Box,   Empty, Wall),
        ArraySeq(Wall,  Wall,  Robot, Empty, Box,   Empty, Empty, Wall),
        ArraySeq(Wall,  Empty, Empty, Empty, Box,   Empty, Empty, Wall),
        ArraySeq(Wall,  Empty, Wall,  Empty, Box,   Empty, Empty, Wall),
        ArraySeq(Wall,  Empty, Empty, Empty, Box,   Empty, Empty, Wall),
        ArraySeq(Wall,  Empty, Empty, Empty, Empty, Empty, Empty, Wall),
        ArraySeq(Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall,  Wall)
      ),
      List(Left, Up, Up, Right, Right, Right, Down, Down, Left, Down, Right, Right, Down, Left, Left)
    ),
    teardown = _ => ()
  )

  simulation.test("Verify Simulation.toString() returns string representation"): s =>
    assertEquals(
      s.toString,
      """########
        |#..O.O.#
        |##@.O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |
        |<^^>>>vv<v>>v<<""".stripMargin
    )

  simulation.test("Day15.read() reads and parses simulation"): s =>
    assertEquals(Day15.read(os.pwd / "day15" / "test1.txt"), s)

  simulation.test("s.robot returns coordinates of robot"): s =>
    assertEquals(s.robot, Point(2, 2))

  simulation.test("s.robot fails if there is more than one robot"): s =>
    s.cs(Point(3, 3)) = Robot
    intercept[AssertionError](s.robot)

  simulation.test("s.robot fails if there are no robots"): s =>
    s.cs(Point(2, 2)) = Empty
    intercept[AssertionError](s.robot)

  simulation.test("s.move can move robot into empty cell"): s =>
    val r1 = s.move(Down, s.robot)
    assertEquals(r1, true)
    assertEquals(s.robot, Point(2, 3))
    assertEquals(s.cs(Point(2, 2)), Empty)
    assertEquals(s.cs(Point(2, 3)), Robot)

    val r2 = s.move(Right, s.robot)
    assertEquals(r2, true)
    assertEquals(s.robot, Point(3, 3))
    assertEquals(s.cs(Point(2, 3)), Empty)
    assertEquals(s.cs(Point(3, 3)), Robot)

    val r3 = s.move(Up, s.robot)
    assertEquals(r3, true)
    assertEquals(s.robot, Point(3, 2))
    assertEquals(s.cs(Point(3, 3)), Empty)
    assertEquals(s.cs(Point(3, 2)), Robot)

    val r4 = s.move(Left, s.robot)
    assertEquals(r4, true)
    assertEquals(s.robot, Point(2, 2))
    assertEquals(s.cs(Point(3, 2)), Empty)
    assertEquals(s.cs(Point(2, 2)), Robot)

  simulation.test("s.robot cannot move robot into wall"): s =>
    assertEquals(s.move(Left, s.robot), false)
    assertEquals(s.robot, Point(2, 2))

  simulation.test("s.robot moves box" ): s =>
    assertEquals(s.move(Right, s.robot), true)
    assertEquals(s.robot, Point(3, 2))
    assertEquals(s.move(Right, s.robot), true)
    assertEquals(s.robot, Point(4, 2))
    assertEquals(s.cs(Point(5, 2)), Box)
    assertEquals(s.move(Down, s.robot), true)
    assertEquals(s.robot, Point(4, 3))
    (4 to 6).foreach(y => assertEquals(s.cs(Point(4, y)), Box))
    assertEquals(s.move(Down, s.robot), false)
