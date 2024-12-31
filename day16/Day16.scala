package aoc2024.day16

import aoc2024.Direction
import aoc2024.Location
import aoc2024.Log
import aoc2024.Point

enum Tile(var code: Char):
  case Empty extends Tile('.')
  case Start extends Tile('S')
  case End extends Tile('E')
  case Wall extends Tile('#')

case class Maze(m: Vector[Vector[Tile]]):

  def find(p: Tile => Boolean): Point =
    val ts = m.zipWithIndex
      .flatMap: (r, y) =>
        r.zipWithIndex.collect:
          case (t, x) if p(t) => Point(x, y)
    assert(ts.size == 1)
    ts.head

  def nextSteps(l: Location): List[(Location, Int)] =
    List(
      (l.step, 1),
      (l.turnLeft.step, 1000),
      (l.turnRight.step, 1000)
    ).filter((l, _) => m(l) != Tile.Wall)

  def bestPath(): Int =
    def bestPath1(l: Location, score: Int, cache: Map[Location, Int]): (Int, Map[Location, Int]) =
      Log.log(s"bestPath1(l=$l, score=$score, cache =$cache)"):
        if cache.contains(l) then
          val s = score min cache(l)
          (s, cache.updated(l, s))
        else if m(l) == Tile.End then (score, cache.updated(l, score))
        else
          nextSteps(l)
            .foldLeft((Int.MaxValue, Map(l -> Int.MaxValue))): (b, a) =>
              val (s, c) = bestPath1(a(0), score + a(1), b(1))
              (b(0) min s, (b(1) ++ c).groupMapReduce(_(0))(_(1))(_ min _))
    bestPath1(Location(this.find(_ == Tile.Start), Direction.East), 0, Map.empty)(0)

  override def toString(): String =
    val b = StringBuilder()
    m.foreach: r =>
      b ++= r.map(_.code).mkString
      b += '\n'
    b.result()

object Day16:

  def read(p: os.Path): Maze =
    val m = os
      .read(p)
      .linesIterator
      .map(_.map(c => Tile.values.find(c == _.code).get).toVector)
      .toVector
    Maze(m)
