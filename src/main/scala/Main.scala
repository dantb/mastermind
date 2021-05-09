import model.Mastermind
import model.Mastermind._
import typeclasses.Show._
import effects._

@main def game: Unit = {

  IO.runIO {
    Console[IO].read
  }

}

