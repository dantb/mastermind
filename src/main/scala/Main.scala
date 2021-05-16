import effects._
import effects.IO.given
import effects.syntax._
import model.Mastermind
import model.Mastermind._
import typeclasses._
import typeclasses.syntax.monad._

@main def game: Unit = {

  IO.runIO {
    for {
      input <- Console[IO].read
      board <- IO.fromEither(Board.parseInitialBoard(input).left.map(s => new Exception(s)))
      _     <- s"Board is: ${Show[Board].show(board)}".writeLn
    } yield ()
  }

}
