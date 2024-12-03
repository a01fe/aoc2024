package aoc2024

sealed trait Instruction
case class Mul(x: Int, y: Int) extends Instruction:
  def result = x * y
case class Do() extends Instruction
case class DoNot() extends Instruction

object Day3:

  def parsePartOne(p: os.Path): Iterator[Mul] =
    val s = os.read(p)
    val mulPattern = raw"mul\((\d{1,3}),(\d{1,3})\)".r.unanchored
    mulPattern
      .findAllMatchIn(s)
      .map(m => Mul(m.group(1).toInt, m.group(2).toInt))

  def partOne(p: os.Path): Int =
    parsePartOne(p).map(_.result).sum

  def parsePartTwo(p: os.Path): Iterator[Instruction] =
    val s = os.read(p)
    val instructionPattern = raw"(?:(mul)\((\d{1,3}),(\d{1,3})\))|(?:(do)\(\))|(?:(don't)\(\))".r.unanchored
    instructionPattern
      .findAllMatchIn(s)
      .map(_.subgroups)
      .collect:
        case List("mul", x, y, _*)                     => Mul(x.toInt, y.toInt)
        case List(null, null, null, "do", _*)          => Do()
        case List(null, null, null, null, "don't", _*) => DoNot()

  def evaluate(is: List[Instruction], enabled: Boolean = true): Int =
    (is: @unchecked) match
      case Nil                       => 0
      case Do() :: t                 => evaluate(t, true)
      case DoNot() :: t              => evaluate(t, false)
      case (m: Mul) :: t if enabled  => m.result + evaluate(t, enabled)
      case (m: Mul) :: t if !enabled => evaluate(t, enabled)

  def run(p: os.Path) =
    println(s"part 1: ${{ partOne(p) }}")
    val is = parsePartTwo(p).toList
    println(s"part 2: ${{ evaluate(is) }}")