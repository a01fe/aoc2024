package aoc2024.day10

import aoc2024.Direction.*
import aoc2024.Log.log
import aoc2024.Point

case class TrailMap(val as: Vector[Vector[Int]]):

  val max = Point(as(0).size, as.size)

  def stepsTo(a: Int, p: Point): List[Point] =
    List(North, South, East, West)
      .map(p + _.delta)
      .filter(p => p.inside(Point(0, 0), max))
      .filter(p => as(p.y)(p.x) == a)

  def trailheads: List[Point] =
    as.zipWithIndex
      .map: (xs, y) =>
        xs.zipWithIndex.collect:
          case (0, x) => Point(x, y)
      .flatten
      .toList

  def countHikes(): Int =
    trailheads.map(countHike(_)).sum

  def countHike(p: Point): Int =
    def countHike1(p: Point, v: Set[Point]): (Int, Set[Point]) =
      val a = as(p.y)(p.x)
      log(s"countHike($p) a=$a v=$v"):
        if a == 9 then (1, v + p)
        else
          stepsTo(a + 1, p)
            .filterNot(v(_))
            .foldLeft((0, v + p)): (s, p) =>
              val r = countHike1(p, s(1))
              (s(0) + r(0), s(1) ++ r(1))
    countHike1(p, Set.empty[Point])(0)

  def ratings(): Int =
    trailheads.map(rating(_)).sum

  def rating(p: Point): Int =
    def rating1(p: Point): Int =
      val a = as(p.y)(p.x)
      log(s"rating1($p) a=$a"):
        if a == 9 then 1
        else
          stepsTo(a + 1, p)
            .map(rating1(_))
            .sum
    rating1(p)

object Day10:
  def read(p: os.Path): TrailMap =
    val as = os
      .read(p)
      .linesIterator
      .map(_.map(c => (c - '0').toInt).toVector)
      .toVector
    TrailMap(as)
