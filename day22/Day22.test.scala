package aoc2024.day22

class TestDay22 extends munit.FunSuite:

  test("Verify nextSecret() returns next secret number in sequence"):
    assertEquals(123L.nextSecret(), 15887950L)
    assertEquals(15887950L.nextSecret(), 16495136L)
    assertEquals(16495136L.nextSecret(), 527345L)

  test("Verify nthSecret(n) returns nth secret number in sequence"):
    assertEquals(123L.nthSecret(10), 5908254L)

  test("Verify Day22.read() reads list of initial secret numbers"):
    val e = List(1L, 10L, 100L, 2024L)
    val o = Day22.read(os.pwd / "day22" / "test.txt")
    assertEquals(o, e)

  test("Verify Day22.sumOfSecrets returns expected result"):
    val bs = Day22.read(os.pwd / "day22" / "test.txt")
    assertEquals(Day22.sumOfSecrets(bs, 2000), 37327623L)
