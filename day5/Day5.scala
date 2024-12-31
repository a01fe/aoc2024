package aoc2024.day5

import scala.annotation.tailrec

case class Rule(b: Int, a: Int)

final class Rules(rs: List[Rule]):
  def hasMatch(b: Int, a: Int): Boolean =
    rs.exists(r => r.b == b && r.a == a)

  def isValidOrder(b: Int, a: Int): Boolean =
    if hasMatch(b, a) then true
    else if hasMatch(a, b) then false
    else true

  def isValidPage(p: Int, us: List[Int]): Boolean =
    us.forall(p1 => isValidOrder(p, p1))

  @tailrec
  def isValidUpdate(us: List[Int]): Boolean =
    us match
      case Nil    => true
      case h :: t => isValidPage(h, t) && isValidUpdate(t)

  def fixUpdate(us: List[Int]): List[Int] =
    def fixPage(p: Int, us: List[Int]): (Boolean, List[Int]) =
      us match
        case Nil => (false, p :: Nil)
        case _ =>
          val (good, bad) = us.span(isValidOrder(p, _))
          bad match
            case Nil =>
              val r = fixPage(good.head, good.tail)
              (r(0), p :: r(1))
            case h :: t =>
              (true, h :: good ::: p :: t)
    us match
      case Nil => Nil
      case h :: t =>
        fixPage(h, t) match
          case (false, r) => r
          case (true, r)  => fixUpdate(r)

  override def toString(): String =
    s"Rules(${rs.map(_.toString()).mkString(",")})"

object Day5:

  def readRules(i: Iterator[String]): Rules =
    @tailrec
    def readRules1(i: Iterator[String], rs: List[Rule]): List[Rule] =
      val p = raw"(\d+)\|(\d+)".r
      i.next() match
        case ""      => rs
        case p(b, a) => readRules1(i, Rule(b.toInt, a.toInt) :: rs)
    Rules(readRules1(i, Nil))

  def readUpdates(i: Iterator[String]): List[List[Int]] =
    @tailrec
    def readUpdates1(i: Iterator[String], us: List[List[Int]]): List[List[Int]] =
      if !i.hasNext then us
      else readUpdates1(i, i.next().split(",").map(_.toInt).toList :: us)
    readUpdates1(i, Nil)

  def read(p: os.Path): (Rules, List[List[Int]]) =
    val i = os.read(p).linesIterator
    (readRules(i), readUpdates(i))

  def part1(rules: Rules, updates: List[List[Int]]): Int =
    updates
      .filter(rules.isValidUpdate(_))
      .map(u => u((u.size - 1) / 2))
      .sum

  def part2(rules: Rules, updates: List[List[Int]]): Int =
    updates
      .filter(!rules.isValidUpdate(_))
      .map(rules.fixUpdate(_))
      .map(u => u((u.size - 1) / 2))
      .sum

  def run(p: os.Path): Unit =
    val (rules, updates) = read(p)
    println(s"part 1: ${part1(rules, updates)}")
    println(s"part 2: ${part2(rules, updates)}")
