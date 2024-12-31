package aoc2024.day7

import scala.annotation.tailrec

enum Op(val isPart2: Boolean):
  case Add extends Op(isPart2 = false)
  case Mul extends Op(isPart2 = false)
  case Cat extends Op(isPart2 = true)

  def eval(a: Long, b: Long): Long =
    this match
      case Add => a + b
      case Mul => a * b
      case Cat => (a.toString() + b.toString()).toLong

object Op:
  def prod(as: List[Op], bs: List[List[Op]]): List[List[Op]] =
    as.flatMap: a =>
      bs.map(b => (a :: b))

  def pow(l: List[Op], n: Long): List[List[Op]] =
    def pow1(n: Long, p: List[List[Op]]): List[List[Op]] =
      n match
        case 1 => p
        case _ => pow1(n - 1, prod(l, p))
    pow1(n, l.map(List(_)))

case class Calibration(total: Long, params: List[Long]):

  def eval(ops: List[Op]) =
    @tailrec
    def eval1(total: Long, params: List[Long], ops: List[Op]): Long =
      params match
        case Nil    => total
        case h :: t => eval1(ops.head.eval(total, h), t, ops.tail)
    eval1(params.head, params.tail, ops)

extension (cs: List[Calibration])
  def totalCalibrationResult(isPart2: Boolean): Long =
    cs
      .filter: c =>
        val l = Op.values.toList.filterNot(!isPart2 && _.isPart2)
        Op.pow(l, c.params.size).map(c.eval(_)).exists(_ == c.total)
      .map(_.total)
      .sum

object Day7:

  def read(p: os.Path): List[Calibration] =
    val calibrationPattern = raw"(\d+):\s+((?:\d+\s*)+)".r
    os.read(p)
      .linesIterator
      .map: l =>
        l match
          case calibrationPattern(total, params) =>
            Calibration(total.toLong, params.split(raw"\s+").map(_.toLong).toList)
      .toList

  def run(p: os.Path): Unit =
    val cs = Day7.read(p)
    println(s"part 1: ${cs.totalCalibrationResult(isPart2 = false)}")
    println(s"part 2: ${cs.totalCalibrationResult(isPart2 = true)}")
