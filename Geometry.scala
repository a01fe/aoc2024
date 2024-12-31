package aoc2024

import scala.collection.mutable.ArraySeq

case class Point(x: Int, y: Int):
  def +(v: Int) = Point(x + v, y + v)
  def +(v: Point) = Point(x + v.x, y + v.y)
  def +(v: Location) = Point(x + v.p.x, y + v.p.y)
  def -(v: Int) = Point(x - v, y - v)
  def -(v: Point) = Point(x - v.x, y - v.y)
  def -(v: Location) = Point(x - v.p.x, y - v.p.y)
  def *(v: Int) = Point(x * v, y * v)
  def *(v: Point) = Point(x * v.x, y * v.y)
  def *(v: Location) = Point(x * v.p.x, y * v.p.y)
  def /(v: Int) = Point(x / v, y / v)
  def /(v: Point) = Point(x / v.x, y / v.y)
  def /(v: Location) = Point(x / v.p.x, y / v.p.y)
  def %(v: Int) = Point(x % v, y % v)
  def %(v: Point) = Point(x % v.x, y % v.y)
  def %(v: Location) = Point(x % v.p.x, y % v.p.y)
  def inside(lb: Point, ub: Point): Boolean =
    this.x >= lb.x && this.x < ub.x
      && this.y >= lb.y && this.y < ub.y
  override def toString(): String = s"($x,$y)"

object Point:

  extension [T](m: Vector[Vector[T]]) def apply(p: Point): T = m(p.y)(p.x)

  extension [T](m: ArraySeq[ArraySeq[T]])
    def apply(p: Point): T = m(p.y)(p.x)
    def update(p: Point, elem: T): Unit = m(p.y)(p.x) = elem

// object Point:
//   import scala.math.Ordering.Implicits.infixOrderingOps
//   given Ordering[Point] with
//     def compare(p1: Point, p2: Point) =
//       (p1.x.compare(p2.x), p1.y.compare(p2.y)) match
//         case (0, 0) => 0
//         case (a, b) if a >= 0 && b >= 0 => 1
//         case _ => -1

enum Direction(val delta: Point):
  case North extends Direction(Point(0, -1))
  case South extends Direction(Point(0, 1))
  case East extends Direction(Point(1, 0))
  case West extends Direction(Point(-1, 0))

  def turnLeft: Direction =
    this match
      case North => West
      case West  => South
      case South => East
      case East  => North

  def turnRight: Direction =
    this match
      case North => East
      case East  => South
      case South => West
      case West  => North

object Direction:
  given directionToPoint: Conversion[Direction, Point] = _.delta

case class Location(val p: Point, d: Direction):
  def turnLeft: Location = Location(p, d.turnLeft)
  def turnRight: Location = Location(p, d.turnRight)
  def step: Location = Location(p + d.delta, d)

object Location:
  extension [T](m: Vector[Vector[T]]) def apply(l: Location): T = m(l.p.y)(l.p.x)

  extension [T](m: ArraySeq[ArraySeq[T]])
    def apply(l: Location): T = m(l.p.y)(l.p.x)
    def update(l: Location, elem: T): Unit = m(l.p.y)(l.p.x) = elem
