import model.Mastermind
import model.Mastermind._
import typeclasses.Show._

@main def hello: Unit = {
    println("Hello world!")
    println(msg)

    val tuple: Tuple2[Int, Int] = (1, 2)
    println(s"Tuple $tuple is instance of tuple? ${tuple.isInstanceOf[Tuple]}")
    println(s"Tuple $tuple is instance of tuple 2? ${tuple.isInstanceOf[Tuple2[Int, Int]]}")

//    val thing = summonInline[Mirror.Of[Tuple]]
//    val thing: Mirror.Of[Tuple] = tuple

    println(CodePeg.Red.show)
    println(KeyPeg.Black.show)

    val board: Board = initialise(CodePeg.Red, CodePeg.Yellow, CodePeg.Red, CodePeg.Green)

    println(board.show)




}

def msg = "I was compiled by Scala 3. :)"
