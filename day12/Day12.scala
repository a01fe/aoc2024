package aoc2024.day12

case class Region()

object Day12:
  def read(p: os.Path): Vector[Vector[Char]] =
    os.read(p)
      .linesIterator
      .map: l =>
        l.toVector
      .toVector
