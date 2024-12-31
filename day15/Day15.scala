package aoc2024.day15

import scala.annotation.tailrec
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ListBuffer

import aoc2024.Point

enum Cell(val symbol: Char):
  case Empty extends Cell('.')
  case Box extends Cell('O')
  case Wall extends Cell('#')
  case Robot extends Cell('@')

enum Move(val symbol: Char, val delta: Point):
  case Up extends Move('^', Point(0, -1))
  case Down extends Move('v', Point(0, 1))
  case Right extends Move('>', Point(1, 0))
  case Left extends Move('<', Point(-1, 0))

class Simulation(val cs: ArraySeq[ArraySeq[Cell]], val ms: List[Move]):

  def robot =
    val r = cs.zipWithIndex
      .flatMap: (xs, y) =>
        xs.zipWithIndex.collect:
          case (Cell.Robot, x) => Point(x, y)
    assert(r.size == 1)
    r.head

  def move(m: Move, p: Point): Boolean =
    val c = cs(p)
    val to = p + m.delta
    cs(to) match
      case Cell.Empty =>
        cs(to) = c
        cs(p) = Cell.Empty
        true
      case Cell.Box =>
        if move(m, to) then
          cs(to) = c
          cs(p) = Cell.Empty
          true
        else false
      case _ => false

  def run(): Unit =
    @tailrec
    def run1(ms: List[Move]): Unit =
      ms match
        case Nil    => ()
        case h :: t => move(h, robot); run1(t)
    run1(ms)

  def boxScore(): Int =
    cs.zipWithIndex
      .flatMap: (xs, y) =>
        xs.zipWithIndex.collect:
          case (Cell.Box, x) => 100 * y + x
      .sum

  override def equals(that: Any): Boolean =
    that match
      case t: Simulation if cs == t.cs && ms == t.ms => true
      case _                                         => false

  override def hashCode(): Int = cs.hashCode() + ms.hashCode()

  override def toString(): String =
    val b = StringBuilder()
    cs.foreach: r =>
      b ++= r.map(_.symbol).mkString
      b += '\n'
    b += '\n'
    b ++= ms.grouped(1000).map(_.map(_.symbol).mkString).mkString("\n")
    b.toString()

object Day15:

  def read(p: os.Path): Simulation =
    val i = os.read(p).linesIterator.buffered

    val cb = ArraySeq.newBuilder[ArraySeq[Cell]]
    while i.headOption.exists(_ != "") do cb += i.next().map(c => Cell.values.find(_.symbol == c).get).toArray

    val lb = ListBuffer.empty[Move]
    for l <- i do lb ++= l.map(c => Move.values.find(_.symbol == c).get)

    Simulation(cb.result(), lb.result())

  def run(p: os.Path): Unit =
    val simulation = Day15.read(p)
    simulation.run()
    println(s"part 1: ${simulation.boxScore()}")
