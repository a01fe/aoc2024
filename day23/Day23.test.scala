package aoc2024.day23

class TestDay23 extends munit.FunSuite:

  test("Verify Day23.read() reads list of connections"):
    val e = LanParty(
      List(
        Connection(Host("kh"), Host("tc")),
        Connection(Host("qp"), Host("kh")),
        Connection(Host("de"), Host("cg"))
      )
    )
    val o = Day23.read(os.pwd / "day23" / "test1.txt")
    assertEquals(o, e)

  test("Keys of hostConnections are all hosts"):
    val e = Set(Host("kh"), Host("tc"), Host("qp"), Host("de"), Host("cg"))
    val l = Day23.read(os.pwd / "day23" / "test1.txt")
    val o = l.hostConnections().keySet
    assertEquals(o, e)

  test("hostConnections returns connections for each host"):
    val l = Day23.read(os.pwd / "day23" / "test1.txt")
    val e_kh = Set(
      Connection(Host("kh"), Host("tc")),
      Connection(Host("qp"), Host("kh"))
    )
    val o_kh = l.hostConnections()(Host("kh"))
    assertEquals(o_kh, e_kh)
