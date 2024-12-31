package aoc2024.day9

import scala.collection.mutable

sealed trait Block:
  def isFree: Boolean

case class Free() extends Block:
  def isFree = true

case class File(id: Int) extends Block:
  def isFree = false

class DiskMap(blocks: Array[Block]):

  def compact(): Unit =
    def compact1(firstFree: Int, lastUsed: Int): Unit =
      (firstFree, lastUsed) match
        case (-1, _) | (_, -1) => ()
        case (f, l) if f > l   => ()
        case (f, l) =>
          val t = blocks(f)
          blocks(f) = blocks(l)
          blocks(l) = t
          compact1(blocks.indexWhere(_.isFree, f), blocks.lastIndexWhere(!_.isFree, l))
    compact1(blocks.indexWhere(_.isFree), blocks.lastIndexWhere(!_.isFree))

  def checksum: Long =
    blocks.zipWithIndex
      .collect:
        case (File(id), i) => id.toLong * i
      .sum

object Day9:
  def read(p: os.Path): DiskMap =
    val blocks = mutable.ArrayBuffer.empty[Block]
    def readPairs(i: Iterator[Char], id: Int): Unit =
      val c1 = i.next()
      if c1.isDigit then
        blocks ++= Array.fill(c1 - '0')(File(id))
        val c2 = i.next()
        if c2.isDigit then
          blocks ++= Array.fill(c2 - '0')(Free())
          readPairs(i, id + 1)
    readPairs(os.read(p).toIterator, 0)
    DiskMap(blocks.toArray)

  def run(p: os.Path): Unit =
    val diskMap = read(p)
    diskMap.compact()
    println(s"part1: ${diskMap.checksum}")
