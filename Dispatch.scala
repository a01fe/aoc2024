package aoc2024

object Dispatch:

  @main def main(args: String*) =
    args match
      case Seq() => println("usage: dispatch day datafile")
      case Seq(day, file) =>
        val p = os.pwd / day / file
        day match
          case "day1" => Day1.run(p)
          case "day2" => Day2.run(p)
          case "day3" => Day3.run(p)
