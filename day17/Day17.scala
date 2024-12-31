package aoc2024.day17

import scala.sys.error
import aoc2024.Log.log
import scala.annotation.tailrec

type Word = Long

extension (n: Long)
  infix def **(e: Long): Long =
    e match
      case 0          => 1
      case 1          => n
      case x if x < 0 => error(s"cannot raise to negative power $n")
      case x          => n * (n ** (e - 1))

class Cpu(
  val m: Vector[Byte],
  var regA: Word,
  var regB: Word,
  var regC: Word
):
  var ic: Int = 0

  lazy val size = m.size

  inline def fetch: Byte =
    val b = m(ic)
    ic += 1
    b

  inline def literal: Word =
    log(s"fetch literal ic=$ic"):
      fetch

  inline def combo: Word =
    log(s"fetch combo ic=$ic"):
      fetch match
        case 4           => regA
        case 5           => regB
        case 6           => regC
        case x if x >= 7 => error(s"Invalid combo operand $x at program location ${ic - 1}")
        case x           => x

  def step(): Option[Byte] =
    log(s"step ic=$ic, regA=$regA, regB=$regB, regC=$regC, m=${if ic < size then m(ic).toString() else "-"}"):
      fetch match
        case 0 => regA = regA / (2 ** combo); None // adv
        case 1 => regB = regB ^ literal; None // bxl
        case 2 => regB = combo & 7; None // bst
        case 3 => if regA != 0 then ic = literal.toInt else fetch; None // jmz
        case 4 => ic += 1; regB = regB ^ regC; None // bxc
        case 5 => Some((combo & 7).toByte) // out
        case 6 => regB = regA / (2 ** combo); None // bdv
        case 7 => regC = regA / (2 ** combo); None // cdv
        case x => error(s"Invalid value $x at program location ${ic - 1}")

  def run(startingIc: Int = 0): List[Byte] =
    var output = List.empty[Byte]
    ic = startingIc
    while ic < size do step().foreach(o => output = o :: output)
    output

  def runUntilOutput(): Option[Byte] =
    var output = Option.empty[Byte]
    while ic < size && output.isEmpty do output = step()
    output

  def isOutputMatch(a: Word, pattern: List[Byte]): Boolean =
    @tailrec
    def isOutputMatch1(p: List[Byte]): Boolean =
      runUntilOutput() match
        case None if p == Nil                   => true
        case Some(o) if p != Nil && o == p.head => isOutputMatch1(p.tail)
        case _                                  => false
    regA = a
    regB = 0
    regC = 0
    ic = 0
    isOutputMatch1(pattern)

  def disassemble(): String =
    def disassembleCombo(i: Int, b: StringBuilder): Unit =
      m(i) match
        case 4           => b ++= "A"
        case 5           => b ++= "B"
        case 6           => b ++= "C"
        case x if x >= 7 => b ++= s"??? ($x)"
        case x           => b ++= x.toString()

    @tailrec
    def disassembleOne(i: Int, b: StringBuilder): String =
      if i >= size then b.toString()
      else
        val (op, arg) = (m(i), m(i + 1))
        b ++= f"$i%3d $op%3d $arg%3d  "
        m(i) match
          case 0 => b ++= "adv "; disassembleCombo(i + 1, b)
          case 1 => b ++= "bxl "; b ++= m(i + 1).toString()
          case 2 => b ++= "bst "; disassembleCombo(i + 1, b)
          case 3 => b ++= "jnz "; b ++= m(i + 1).toString()
          case 4 => b ++= "bxc "
          case 5 => b ++= "out "; disassembleCombo(i + 1, b)
          case 6 => b ++= "bdv "; disassembleCombo(i + 1, b)
          case 7 => b ++= "cdv "; disassembleCombo(i + 1, b)
          case x => b ++= s"??? (${m(i)},$m{i + 1})"
        b ++= "\n"
        disassembleOne(i + 2, b)
    disassembleOne(0, StringBuilder())

  override def toString(): String =
    s"Cpu(m=${m.mkString(",")}, regA=$regA, regB=$regB, regC=$regC, ic=$ic)"

object Day17:

  def read(p: os.Path): Cpu =
    val regAPattern = raw"Register A: (\d+)".r
    val regBPattern = raw"Register B: (\d+)".r
    val regCPattern = raw"Register C: (\d+)".r
    val programPattern = raw"Program: ([0-9,]+)".r
    val blankPattern = raw"\s*".r
    var regA: Option[Word] = None
    var regB: Option[Word] = None
    var regC: Option[Word] = None
    var program: Option[Vector[Byte]] = None
    val lines = os.read(p).linesIterator
    while lines.hasNext do
      lines.next() match
        case regAPattern(a)    => regA = Some(a.toInt)
        case regBPattern(b)    => regB = Some(b.toInt)
        case regCPattern(c)    => regC = Some(c.toInt)
        case programPattern(p) => program = Some(p.split(",").map(_.toByte).toVector)
        case blankPattern      => ()
    Cpu(program.get, regA.get, regB.get, regC.get)
