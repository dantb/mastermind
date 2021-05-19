package typeclasses

import org.junit.Assert.assertEquals
import org.junit.Test

class TraverseSpec {
  @Test def sequenceListSomes(): Unit = {
    val input: List[Option[Int]] = List(Option(1), Option(2), Option(3))
    val output: Option[List[Int]] = Traverse[List].sequence(input)
    assertEquals(output, Option(List(1, 2, 3)))
  }

  @Test def sequenceListNone(): Unit = {
    val input: List[Option[Int]] = List(Option(1), None, Option(3))
    val output: Option[List[Int]] = Traverse[List].sequence(input)
    assertEquals(output, None)
  }

  @Test def traverseListSomes(): Unit = {
    val input: List[Int] = List(1, 2, 3)
    val output: Option[List[String]] = Traverse[List].traverse(input)(i => Option((i + 1).toString))
    assertEquals(output, Option(List("2", "3", "4")))
  }

  @Test def traverseListNone(): Unit = {
    val input: List[Int] = List(1, 2, 3)
    val output: Option[List[String]] = Traverse[List].traverse(input)(i => if (i % 2 == 0) None else Option((i + 1).toString))
    assertEquals(output, None)
  }
}
