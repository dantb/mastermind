
import org.junit.Test
import org.junit.Assert._
import typeclasses.Show.show
import model._

import typeclasses.syntax.syntax._

class TestSyntax {
  @Test def testBoardShowSyntax(): Unit = {
    val board: Board = Board.initialise(CodePeg.Red, CodePeg.Yellow, CodePeg.Red, CodePeg.Green)
    assertEquals(board.show, "Board(targetRow=List(Red, Yellow, Red, Green), completedRows=List())")
  }
}
