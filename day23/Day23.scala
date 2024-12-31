package aoc2024.day23

case class Host(h: String)
case class Connection(h1: Host, h2: Host)
case class LanParty(cs: List[Connection]):
  def hostConnections(): Map[Host, Set[Connection]] =
    (cs.map(c => (c.h1, c)) ++ cs.map(c => (c.h2, c)))
      .groupMapReduce((h, _) => h)((_, c) => Set(c))((s1, s2) => s1 ++ s2)

object Day23:

  def read(p: os.Path): LanParty =
    val connectionPattern = raw"(\p{Alpha}+)-(\p{Alpha}+)".r
    LanParty(
      os.read(p)
        .linesIterator
        .collect:
          case connectionPattern(h1, h2) => Connection(Host(h1), Host(h2))
        .toList
    )
