package typeclasses

import munit.FunSuite

class TraverseSpec extends munit.FunSuite:
  test("Traverse[List] should sequence a list of 'Some's") {
    val input: List[Option[Int]] = List(Option(1), Option(2), Option(3))
    val output: Option[List[Int]] = Traverse[List].sequence(input)
    assertEquals(output, Option(List(1, 2, 3)))
  }

  test("Traverse[List] should sequence a list with a None in") {
    val input: List[Option[Int]] = List(Option(1), None, Option(3))
    val output: Option[List[Int]] = Traverse[List].sequence(input)
    assertEquals(output, None)
  }

  test("Traverse[List] should traverse a list of 'Some's") {
    val input: List[Int] = List(1, 2, 3)
    val output: Option[List[String]] = Traverse[List].traverse(input)(i => Option((i + 1).toString))
    assertEquals(output, Option(List("2", "3", "4")))
  }

  test("Traverse[List] should traverse a list with a None in") {
    val input: List[Int] = List(1, 2, 3)
    val output: Option[List[String]] = Traverse[List].traverse(input)(i => if (i % 2 == 0) None else Option((i + 1).toString))
    assertEquals(output, None)
  }
