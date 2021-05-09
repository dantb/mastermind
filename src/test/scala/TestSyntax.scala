
import org.junit.Test
import org.junit.Assert._
import typeclasses.Show.show
import model.Mastermind._

import typeclasses.syntax._

class TestSyntax {
  @Test def t1(): Unit = {
    val board: Board = initialise(CodePeg.Red, CodePeg.Yellow, CodePeg.Red, CodePeg.Green)
    assertEquals(board.show, "Board(targetRow=List(Red, Yellow, Red, Green), completedRows=List())")
  }
}
