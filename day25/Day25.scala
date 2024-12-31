package aoc2024.day25

import scala.annotation.tailrec

type Tumbler = List[Int]
inline val PinSize = 5
inline val tumblerDepth = 5

extension (t: Tumbler)
  def overlaps(o: Tumbler): Boolean =
    t.zip(o).map(_ + _).exists(_ > tumblerDepth)

case class LocksKeys(ls: Set[Tumbler], ks: Set[Tumbler]):

  def fitCount: Int =
    ls.flatMap(l => ks.map(k => (l, k)))
      .filterNot((l, k) => l.overlaps(k))
      .size

object Day25:

  def parseTumbler(i: BufferedIterator[String], t: Tumbler): Tumbler =
    i.next()
      .collect:
        case '#' => 1
        case _   => 0
      .zip(t)
      .map(_ + _)
      .toList

  @tailrec
  def parseTumblers(i: BufferedIterator[String], t: Tumbler): Tumbler =
    if i.headOption.forall(_ == "") then
      if i.hasNext then i.next()
      t
    else parseTumblers(i, parseTumbler(i, t))

  def parseKeysAndLocks(i: BufferedIterator[String], s: (Set[Tumbler], Set[Tumbler])): (Set[Tumbler], Set[Tumbler]) =
    if !i.hasNext then s
    else if i.headOption.contains("#" * PinSize) then
      val s1 = (s(0) + parseTumblers(i, List.fill(PinSize)(-1)), s(1))
      parseKeysAndLocks(i, s1)
    else
      val s1 = (s(0), s(1) + parseTumblers(i, List.fill(PinSize)(-1)))
      parseKeysAndLocks(i, s1)

  def read(p: os.Path): LocksKeys =
    val i = os.read(p).linesIterator.buffered
    val (ls, ks) = parseKeysAndLocks(i, (Set.empty[Tumbler], Set.empty[Tumbler]))
    LocksKeys(ls, ks)

  def run(p: os.Path): Unit =
    val lk = read(p)
    println(s"part 1: ${lk.fitCount}")
