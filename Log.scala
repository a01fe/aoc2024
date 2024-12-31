package aoc2024

object Log:

  private inline val logging = true
  private var indent = 0
  private val indentSize = 2

  inline def log[T](msg: => String)(op: => T): T =
    if logging then
      println(s"${" " * indent}$msg: start")
      indent += indentSize
      val result = op
      indent -= indentSize
      println(s"${" " * indent}$msg: result = $result")
      result
    else op
