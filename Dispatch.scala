package aoc2024

object Dispatch:

  @main def main(args: String*) =
    args match
      case Seq() => println("usage: dispatch day datafile")
      case Seq(day, file) =>
        val p = os.pwd / day / file
        day match
          case "day1"  => day1.Day1.run(p)
          case "day2"  => day2.Day2.run(p)
          case "day3"  => day3.Day3.run(p)
          case "day4"  => day4.Day4.run(p)
          case "day5"  => day5.Day5.run(p)
          case "day6"  => day6.Day6.run(p)
          case "day7"  => day7.Day7.run(p)
          case "day8"  => day8.Day8.run(p)
          case "day9"  => day9.Day9.run(p)
          case "day14" => day14.Day14.run(p)
          case "day15" => day15.Day15.run(p)
          case "day24" => day24.Day24.run(p)
          case "day25" => day25.Day25.run(p)
