import aoc2024.day11.Day11
import scala.annotation.tailrec

val ss = Day11.read(os.pwd / "day11" / "test.txt")
// val ss = Day11.read(os.pwd / "day11" / "data.txt")

extension (l: Long)
  def hasEvenDigits: Boolean =
    (l.toString().size % 2) == 0

  def halves: List[Long] =
    val s = l.toString()
    s.splitAt(s.size / 2).toList.map(_.toLong)

def blink(ss: List[Long]): List[Long] =
  ss.flatMap: s =>
    if s == 0 then List(1L)
    else if s.hasEvenDigits then s.halves
    else List(s * 2024)

@tailrec
final def blinks(ss: List[Long], n: Int): List[Long] =
  if n == 0 then ss
  else blinks(blink(ss), n - 1)

// println(blinks(ss, 75).size)
//blinks(List(4), 40).size

val l = blinks(List(0), 33)
l.size
val d = l.distinct
d.size
