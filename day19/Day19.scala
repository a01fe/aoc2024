package aoc2024.day19

case class Designs(patterns: List[String], designs: List[String]):

  def isDesignPossible(d: String): Boolean =
    if d == "" then true
    else
      patterns
        .filter(d.startsWith(_))
        .exists: p =>
          isDesignPossible(d.substring(p.size))

  def countPossibleDesigns: Int =
    designs.count(isDesignPossible(_))

  def countDesignPatterns(d: String): Int =
    if d == "" then 1
    else
      patterns
        .filter(d.startsWith(_))
        .map(p => countDesignPatterns(d.substring(p.size)))
        .sum

  def countAllDesignPatterns: Int =
    designs.map(countDesignPatterns(_)).sum

object Day19:

  def read(p: os.Path): Designs =
    val lines = os.read(p).linesIterator
    val patterns = lines.next().split(raw",\s*").toList
    assert(lines.next() == "")
    val designs = lines.toList
    Designs(patterns, designs)
