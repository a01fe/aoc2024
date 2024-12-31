package aoc2024.day25

import aoc2024.day25.*

class TestDay25 extends munit.FunSuite:

  test("overlaps returns false for locks and keys that do not overlap"):
    val l1 = List(0, 5, 3, 4, 3)
    val k1 = List(3, 0, 2, 0, 1)
    assertEquals(l1.overlaps(k1), false)
    val l2 = List(1, 2, 0, 5, 3)
    val k2 = List(4, 3, 4, 0, 2)
    assertEquals(l2.overlaps(k2), false)
    val l3 = List(1, 2, 0, 5, 3)
    val k3 = List(3, 0, 2, 0, 1)
    assertEquals(l3.overlaps(k3), false)

  test("overlaps returns true for locks and keys that do overlap"):
    val l1 = List(0, 5, 3, 4, 3)
    val k1 = List(5, 0, 2, 1, 3)
    assertEquals(l1.overlaps(k1), true)
    val l2 = List(0, 5, 3, 4, 3)
    val k2 = List(4, 3, 4, 0, 2)
    assertEquals(l2.overlaps(k2), true)
    val l3 = List(1, 2, 0, 5, 3)
    val k3 = List(5, 0, 2, 1, 3)
    assertEquals(l3.overlaps(k3), true)

  test("parseTumbler returns ones for '#' pins"):
    val l = List.fill(PinSize)(0)
    val i = "#####".linesIterator.buffered
    assertEquals(Day25.parseTumbler(i, l), List.fill(PinSize)(1))

  test("parseTumbler returns zeros for '.' pins"):
    val l = List.fill(PinSize)(0)
    val i = ".....".linesIterator.buffered
    assertEquals(Day25.parseTumbler(i, l), List.fill(PinSize)(0))

  test("parseTumbler adds pins to initial list"):
    val l = List.tabulate(PinSize)(i => i + 1)
    val i1 = "#.#.#".linesIterator.buffered
    assertEquals(Day25.parseTumbler(i1, l), List(2, 2, 4, 4, 6))
    val i2 = ".#.#.".linesIterator.buffered
    assertEquals(Day25.parseTumbler(i2, l), List(1, 3, 3, 5, 5))

  test("parseTumblers returns initial list if iterator is exhausted"):
    val l = List.fill(PinSize)(0)
    val i = "".linesIterator.buffered
    assertEquals(Day25.parseTumblers(i, l), l)

  test("parseTumblers returns initial list if iterator at empty line"):
    val l = List.fill(PinSize)(0)
    val i = "\n".linesIterator.buffered
    assertEquals(Day25.parseTumblers(i, l), l)

  test("parseTumblers returns sum of tumblers"):
    val l = List.fill(PinSize)(-1)
    val i = """#####
              |#.###
              |#..##
              |#..#.
              |...#.
              |.....
              |""".stripMargin.linesIterator.buffered
    assertEquals(Day25.parseTumblers(i, l), List(3, 0, 1, 4, 2))

  test("parseKeysAndLocks can parse a lock tumbler"):
    val i = """#####
              |#.###
              |#..##
              |#..#.
              |...#.
              |.....
              |""".stripMargin.linesIterator.buffered
    val e = (Set(List(3, 0, 1, 4, 2)), Set.empty[Tumbler])
    assertEquals(Day25.parseKeysAndLocks(i, (Set.empty[Tumbler], Set.empty[Tumbler])), e)

  test("parseKeysAndLocks can parse a key tumbler"):
    val i = """..-#.
              |#..#.
              |#..##
              |##.##
              |#####
              |""".stripMargin.linesIterator.buffered
    val e = (Set.empty[Tumbler], Set(List(3, 1, 0, 4, 2)))
    assertEquals(Day25.parseKeysAndLocks(i, (Set.empty[Tumbler], Set.empty[Tumbler])), e)

  test("Verify read() can read and parse test input"):
    val p = os.pwd / "day25" / "test.txt"
    val e = LocksKeys(
      Set(
        List(0, 5, 3, 4, 3),
        List(1, 2, 0, 5, 3)
      ),
      Set(
        List(5, 0, 2, 1, 3),
        List(4, 3, 4, 0, 2),
        List(3, 0, 2, 0, 1)
      )
    )
    assertEquals(Day25.read(p), e)
