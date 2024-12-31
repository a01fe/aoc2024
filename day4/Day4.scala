package aoc2024.day4

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

type Board = Vector[Vector[Char]]
type Mapping = Board => IndexedSeq[String]

case class WordSearch(board: Board):
  val xMax = board.head.size
  val yMax = board.size
  val mappings = List(
    // rows
    (d: Board) => d.map(_.mkString),
    //  cols
    (d: Board) =>
      (0 until xMax)
        .map(x => (0 until yMax).map(y => d(y)(x)).mkString),
    // positive diagonals (y = x + n)
    (d: Board) =>
      (-(yMax - 1) until yMax)
        .map(n => ((0 max -n) until (xMax min (yMax - n))).map(x => d(x + n)(x)).mkString),
    // negative diagonals (y = n - x)
    (d: Board) =>
      (0 until (2 * yMax - 1))
        .map(n => ((0 max (n - yMax + 1)) until (xMax min (n + 1))).map(x => d(n - x)(x)).mkString)
  )
    .flatMap((f: Mapping) => List(f, (d: Board) => f(d).map(_.reverse)))

  def search(m: Mapping, p: Regex): Seq[Match] =
    m(board).flatMap(p.findAllMatchIn(_))

  def part1: Int =
    val p = "XMAS".r.unanchored
    mappings.map(m => search(m, p).size).sum

  def isMas(x: Int, y: Int): Boolean =
    if board(x)(y) == 'A' then
      (board(x + 1)(y + 1) match
        case 'S' =>
          board(x - 1)(y - 1) match
            case 'M' => true
            case _   => false
        case 'M' =>
          board(x - 1)(y - 1) match
            case 'S' => true
            case _   => false
        case _ => false
      ) && (board(x + 1)(y - 1) match
        case 'S' =>
          board(x - 1)(y + 1) match
            case 'M' => true
            case _   => false
        case 'M' =>
          board(x - 1)(y + 1) match
            case 'S' => true
            case _   => false
        case _ => false
      )
    else false

  def part2: Int =
    (1 until (xMax - 1))
      .map(x =>
        (1 until (yMax - 1))
          .count(y => isMas(x, y))
      )
      .sum

object Day4:
  def read(p: os.Path): WordSearch =
    val board = os
      .read(p)
      .linesIterator
      .map(_.toVector)
      .toVector
    WordSearch(board)

  def run(p: os.Path): Unit =
    val ws = Day4.read(p)
    println(s"part 1: ${ws.part1}")
    println(s"part 2: ${ws.part2}")
