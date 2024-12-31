package aoc2024.day14

import aoc2024.Point

case class Robot(p: Point, v: Point):

  def pAt(t: Int)(using room: Room): Point =
    val pt = (p + (v * t)) % room.ub
    Point(
      if pt.x < 0 then room.ub.x + pt.x else pt.x,
      if pt.y < 0 then room.ub.y + pt.y else pt.y
    )

case class Room(rs: Seq[Robot], ub: Point):

  def safetyFactorAt(t: Int): Int =
    given Room = this
    val mid = ub / 2
    rs
      .map(_.pAt(t))
      .collect: p =>
        (p - mid) match
          case Point(x, y) if x < 0 && y < 0 => List(1, 0, 0, 0)
          case Point(x, y) if x < 0 && y > 0 => List(0, 1, 0, 0)
          case Point(x, y) if x > 0 && y < 0 => List(0, 0, 1, 0)
          case Point(x, y) if x > 0 && y > 0 => List(0, 0, 0, 1)
      .reduce((a, b) => a.zip(b).map(_ + _))
      .reduce((a, b) => a * b)

object Day14:

  def read(p: os.Path): Room =
    val pvPattern = raw"p=(\d+),(\d+) v=(-?\d+),(-?\d+)".r
    val rs = os.read(p)
      .linesIterator
      .collect:
        case pvPattern(px, py, vx, vy) => Robot(Point(px.toInt, py.toInt), Point(vx.toInt, vy.toInt))
      .toSeq
    // This is smelly, but bounds are not part of datasets.
    val ub = if p.last.startsWith("data") then Point(101, 103) else Point(11, 7)
    Room(rs, ub)

  def run(p: os.Path): Unit =
    val room = read(p)
    println(s"part 1: ${room.safetyFactorAt(100)}")
