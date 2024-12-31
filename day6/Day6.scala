package aoc2024.day6

enum Ground(val symbol: Char):
  case Empty extends Ground('.')
  case Obstruction extends Ground('#')

enum Heading(val symbol: Char, val dx: Int, val dy: Int):
  case Up extends Heading('^', 0, -1)
  case Down extends Heading('v', 0, 1)
  case Left extends Heading('<', -1, 0)
  case Right extends Heading('>', 1, 0)
  def turn: Heading =
    this match
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up

object Heading:
  def get(symbol: Char): Option[Heading] =
    Heading.values.find(_.symbol == symbol)

  def apply(symbol: Char): Heading =
    get(symbol).get

object Ground:
  def apply(symbol: Char): Ground =
    Ground.values.find(_.symbol == symbol).get

case class Location(x: Int, y: Int, heading: Heading):
  def step: Location =
    Location(x + heading.dx, y + heading.dy, heading)
  def turn: Location =
    Location(x, y, heading.turn)

case class Room(room: Vector[Vector[Ground]], guard: Location):
  val xMax = room.head.size
  val yMax = room.size
  def step(l: Location): Option[Location] =
    val s = l.step
    if s.x < 0 || s.x >= xMax then None
    else if s.y < 0 || s.y >= yMax then None
    else if room(s.y)(s.x) == Ground.Empty then Some(s)
    else
      val s = l.turn
      step(s)
  def stepper(ls: List[Location]): List[Location] =
    val s = step(ls.head)
    s match
      case None    => ls
      case Some(l) => stepper(l :: ls)

  def part1: Int =
    stepper(guard :: Nil).map(l => (l.x, l.y)).distinct.size

object Day6:

  def read(p: os.Path): Room =
    var guard: Option[Location] = None
    val room = os
      .read(p)
      .linesIterator
      .zipWithIndex
      .map((r, y) =>
        r.zipWithIndex
          .map((c, x) =>
            Heading.get(c) match
              case Some(h) =>
                guard = Some(Location(x, y, h))
                Ground.Empty
              case None => Ground(c)
          )
          .toVector
      )
      .toVector
    Room(room, guard.get)

  def run(p: os.Path): Unit =
    val r = read(p)
    println(s"part 1: ${r.part1}")
