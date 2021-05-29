import effects._
import effects.IO.given
import effects.syntax._
import model.Board
import model.Board
import typeclasses._
import typeclasses.syntax.monad._
import typeclasses.syntax.show._

@main def game: Unit = {

  IO.runIO {
    for {
      _     <- Console[IO].writeLn("What's your name?")
      name  <- Console[IO].read
      _     <- Console[IO].writeLn(s"Hi $name, we're going to play mastermind. We've selected 4 random coloured pegs")
      board <- Board.randomFour[IO]
      _     <- s"Board is: ${Show[Board[4]].show(board)}".writeLn
    } yield ()
  }

}
