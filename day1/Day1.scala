package aoc2024.day1

object Day1:

  def read(p: os.Path): (Vector[Int], Vector[Int]) =
    os.read(p)
      .linesIterator
      .map(_.split(raw"\s+"))
      .collect:
        case Array(l, r) => (l.toInt, r.toInt)
      .toVector
      .unzip match
      case (l, r) => (l.sorted, r.sorted)

  def part1(l: Vector[Int], r: Vector[Int]): Int =
    l.zip(r)
      .map((l, r) => (l - r).abs)
      .sum

  def part2(l: Vector[Int], r: Vector[Int]): Int =
    val rs = r.groupMapReduce(identity)(_ => 1)(_ + _)
    l.map(l => l * rs.getOrElse(l, 0)).sum

  def run(p: os.Path) =
    val (l, r) = read(p)
    println(s"part 1: ${part1(l, r)}")
    println(s"part 2: ${part2(l, r)}")
