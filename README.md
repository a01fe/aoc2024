# AoC2024

Attempts at solving [Advent of Code 2024](https://adventofcode.com/2024) while
(still) learning [Scala](https://www.scala-lang.org/).
Please excuse my awkward code, I'm still getting my Scala legs =^.^=

## Environment

The environment I'm using is:

* macOS 15 on a 13" M2 Air
* Temurin OpenJDK 21 installed via MacPorts
* Visual Studio Code (insiders)
* Metals VSC extension
* [scala-cli](https://scala-cli.virtuslab.org/)

To initialize a new clone, run this inside the project:

```bash
scala-cli setup-ide .
scala-cli compile .
```

Each day's puzzle is in a Scala object in `day`*n*`/Day`*n*`.scala`, typically with test data in `test.txt` and my input file in `data.txt`.

[MUnit](https://scalameta.org/munit/) tests are in `day`*n*`/Day`*n*`.test.scala`.
You can run all tests with:

```bash
scala-cli test .
```

Run a puzzle with, e.g.:

```bash
./dispatch.sh day1 test.txt
```
