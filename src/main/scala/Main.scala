import effects.*
import effects.IO
import effects.syntax.*
import model.Board
import state.StateT
import state.StateT.GameState
import typeclasses.*
import typeclasses.syntax.monad.*
import typeclasses.syntax.show.*

@main def game: Unit =

  val game = StateT.game[IO, 4]

  IO.runIO {
    for {
      _ <- Console[IO].writeLn("What's your name?")
      name <- Console[IO].read
      _ <- Console[IO].writeLn(
        s"Hi $name, we're going to play mastermind. We've selected 4 random coloured pegs"
      )
      board <- Board.randomFour[IO]
      _ <- Console[IO].writeLn(s"Board is: ${Show[Board[4]].show(board)}")
      _ <- game.run(board)
    } yield ()
  }
