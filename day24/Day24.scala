package aoc2024.day24

import scala.collection.mutable

trait Node:
  lazy val value: Boolean

trait Gate extends Node:
  val inputA: Wire
  val inputB: Wire

trait And extends Gate:
  lazy val value = inputA.value && inputB.value

trait Or extends Gate:
  lazy val value = inputA.value || inputB.value

trait XOr extends Gate:
  lazy val value = inputA.value != inputB.value

class Signal(v: Boolean) extends Node:
  lazy val value = v

class Wire(name: String, var source: Option[Node]):
  lazy val value = source.get.value

object Wire:
  private val wires = mutable.Map.empty[String, Wire]

  def apply(name: String, node: Option[Node] = None): Wire =
    wires.getOrElseUpdate(name, new Wire(name, node))

  def apply(name: String, value: Boolean): Wire =
    Wire(name, Some(Signal(value)))

  def value: Long =
    wires
      .collect:
        case (n, w) if n.startsWith("z") => if w.value then (1L << (n.substring(1).toInt)) else 0L
      .reduce((a, b) => a | b)

object Day24:

  def parseSignal(l: String): Option[Wire] =
    val signalPattern = raw"(\w+):\s+([01])".r
    l match
      case signalPattern(n, v) => Some(Wire(n, v == "1"))
      case _                   => None

  def parseGate(l: String): Option[Gate] =
    val gatePattern = raw"(\w+)\s+(AND|OR|XOR)\s+(\w+)\s*->\s*(\w+)".r
    l match
      case gatePattern(a, op, b, o) =>
        val g = op match
          case "AND" => new And { val inputA = Wire(a); val inputB = Wire(b) }
          case "OR"  => new Or { val inputA = Wire(a); val inputB = Wire(b) }
          case "XOR" => new XOr { val inputA = Wire(a); val inputB = Wire(b) }
        Wire(o).source = Some(g)
        Some(g)
      case _ => None

  def read(p: os.Path): Unit =
    val ls = os.read(p).linesIterator
    while ls.hasNext && parseSignal(ls.next()).isDefined do {}
    while ls.hasNext && parseGate(ls.next()).isDefined do {}

  def run(p: os.Path): Unit =
    os.read(p)
    println(s"part 1: ${Wire.value}")
