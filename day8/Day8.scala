package aoc2024.day8

import aoc2024.Point

case class Antenna(freq: Char, loc: Point):
  def antiNodes(that: Antenna): List[Point] =
    if this.freq == that.freq then
      val d = that.loc - this.loc
      (this.loc - d) :: (that.loc + d) :: Nil
    else
      Nil

case class AntennaMap(as: List[Antenna], max: Point):

  def antiNodes =
    as.groupBy(_.freq)
      .map: (_, a) =>
        a.combinations(2)
          .collect:
            case List(a1: Antenna, a2: Antenna) => a1.antiNodes(a2)
          .flatten
          .filter(p => p.inside(Point(0, 0), max))
    .flatten
    .toList
    .distinct

  def part1: Int = antiNodes.size

object Day8:

  def read(p: os.Path): AntennaMap =
    val antennaPattern = raw"[a-zA-Z0-9]".r.unanchored
    var xMax = 0
    var yMax = 0
    val as = os.read(p)
      .linesIterator
      .zipWithIndex
      .map: (l, y) =>
        yMax = (y + 1) max yMax
        xMax = l.size max xMax
        antennaPattern.findAllMatchIn(l)
          .map(m => Antenna(m.matched(0), Point(m.start, y)))
          .toList
      .flatten
      .toList
    AntennaMap(as, Point(xMax, yMax))

  def run(p: os.Path): Unit =
    val antennas = read(p)
    println(s"part1: ${antennas.part1}")
