package aoc2024.day2

import scala.annotation.tailrec

object Day2:

  def read(p: os.Path): List[List[Int]] =
    os.read(p)
      .linesIterator
      .map(_.split(raw"\s+").toList.map(_.toInt))
      .toList

  def isSafe(ls: List[Int]): Boolean =
    @tailrec
    def isSafe2(p: List[List[Int]], o: Int => Boolean): Boolean =
      (p: @unchecked) match
        case Nil             => true
        case List(x, y) :: t => o(y - x) && isSafe2(t, o)
    val p = ls.sliding(2).toList
    (p: @unchecked) match
      case Nil                       => true
      case List(x, y) :: t if y >= x => isSafe2(p, d => d >= 1 && d <= 3)
      case List(x, y) :: t if y < x  => isSafe2(p, d => d >= -3 && d <= -1)

  def isFuzzySafe(ls: List[Int]): Boolean =
    if isSafe(ls) then true
    else
      (0 to ls.size)
        .map(i => ls.take(i) ::: ls.drop(i + 1))
        .exists(isSafe(_))

  def run(p: os.Path) =
    val rs = Day2.read(p)
    println(s"part 1: ${rs.count(isSafe(_))}")
    println(s"part 1: ${rs.count(isFuzzySafe(_))}")
