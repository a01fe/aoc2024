package aoc2024.day11

object Day11:
  def read(p: os.Path): List[Long] =
    os.read(p).linesIterator.next().split(raw"\s").toList.map(_.toLong)
