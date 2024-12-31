import aoc2024.day23.*

val l = Day23.read(os.pwd / "day23" / "test.txt")
// val l = Day23.read(os.pwd / "day23" / "test1.txt")

// l.hostConnections()

// l.hostConnections().keySet.toSeq.combinations(3).toList

val hc = l.hostConnections().toList

hc.combinations(3)
  .filter: trio =>
    trio.sliding(2).forall: pair =>
      val List((h1, c1), (h2, c2)) = pair

      ???


// hc.combinations(3)
//   .map(l => l.map(_(0).h).mkString(" "))
//   .toList
List("aa", "bb", "cc").sliding(2).toList
