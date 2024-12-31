package aoc2024.day4

class TestDay4 extends munit.FunSuite:

  test("Day4.read() reads word search board"):
    val o = Day4.read(os.pwd / "day4" / "test.txt")
    val e = Vector(
      Vector('M', 'M', 'M', 'S', 'X', 'X', 'M', 'A', 'S', 'M'),
      Vector('M', 'S', 'A', 'M', 'X', 'M', 'S', 'M', 'S', 'A'),
      Vector('A', 'M', 'X', 'S', 'X', 'M', 'A', 'A', 'M', 'M'),
      Vector('M', 'S', 'A', 'M', 'A', 'S', 'M', 'S', 'M', 'X'),
      Vector('X', 'M', 'A', 'S', 'A', 'M', 'X', 'A', 'M', 'M'),
      Vector('X', 'X', 'A', 'M', 'M', 'X', 'X', 'A', 'M', 'A'),
      Vector('S', 'M', 'S', 'M', 'S', 'A', 'S', 'X', 'S', 'S'),
      Vector('S', 'A', 'X', 'A', 'M', 'A', 'S', 'A', 'A', 'A'),
      Vector('M', 'A', 'M', 'M', 'M', 'X', 'M', 'M', 'M', 'M'),
      Vector('M', 'X', 'M', 'X', 'A', 'X', 'M', 'A', 'S', 'X')
    )
    assertEquals(o.board, e)
    assertEquals(o.xMax, 10)
    assertEquals(o.yMax, 10)

  test("search finds occurrences of pattern in board"):
    val ws = Day4.read(os.pwd / "day4" / "test.txt")
    val p = "XMAS".r.unanchored
    val o = ws.search((d: Board) => d.map(_.mkString), p).size
    assertEquals(o, 3)

  test("verify partOne on test data returns correct count of matches"):
    val ws = Day4.read(os.pwd / "day4" / "test.txt")
    assertEquals(ws.part1, 18)

  test("verify partTwo on test data returns correct count of MAS matches"):
    val ws = Day4.read(os.pwd / "day4" / "test.txt")
    assertEquals(ws.part2, 9)
