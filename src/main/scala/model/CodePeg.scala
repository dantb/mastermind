package model

import effects.Sync
import typeclasses.Read

import scala.util.Random

enum CodePeg:
  case Red, Green, Blue, Yellow

object CodePeg {
  // compiler warns about unreachable code for string match - this is awesome. Must be using singleton types.
  given Read[CodePeg] = str => str.toLowerCase match {
    case "r" => Some(Red)
    case "g" => Some(Green)
    case "b" => Some(Blue)
    case "y" => Some(Yellow)
    case _ => None
  }

  // grim to hardcode, surely we can get this from the enum ordinality?
  val Size = 4

  def random[F[_]: Sync]: F[CodePeg] = Sync[F].delay {
    val num = new Random().nextInt(Size)
    CodePeg.fromOrdinal(num)
  }
}
