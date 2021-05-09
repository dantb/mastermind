import effects._
import effects.IO.given
import effects.syntax._
import model.Mastermind
import model.Mastermind._
import typeclasses.Show._
import typeclasses.syntax._

@main def game: Unit = {

  IO.runIO {
    for {
      value <- Console[IO].read
      _     <- s"Hey $value".writeLn
    } yield ()
  }

}
