package aoc2024.day22

extension (i: Long)
  def nextSecret(): Long =
    val s1 = ((i * 64) ^ i) % 16777216L
    val s2 = ((s1 / 32) ^ s1) % 16777216L
    ((s2 * 2048) ^ s2) % 16777216L

  def nthSecret(n: Int): Long =
    (1 to n).foldLeft(i)((s, _) => s.nextSecret())

object Day22:

  def read(p: os.Path): List[Long] =
    os.read(p)
      .linesIterator
      .map(_.toLong)
      .toList

  def sumOfSecrets(bs: List[Long], n: Int): Long =
    bs.map(_.nthSecret(n)).sum
