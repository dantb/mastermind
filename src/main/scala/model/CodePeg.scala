package model

import effects.Sync
import typeclasses.{Functor, Randomiser, Read, Show}

import scala.io.AnsiColor
import scala.util.Random

import typeclasses.syntax.functor._

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

  given Show[CodePeg] = new Show[CodePeg]:
    def show(t: CodePeg): String = t match {
      case Red => AnsiColor.RED ++ "O" ++ AnsiColor.RESET
      case Green => AnsiColor.GREEN ++ "O" ++ AnsiColor.RESET
      case Blue => AnsiColor.BLUE ++ "O" ++ AnsiColor.RESET
      case Yellow => AnsiColor.YELLOW ++ "O" ++ AnsiColor.RESET
    }

  // grim to hardcode, surely we can get this from the enum ordinality?
  val Size: 4 = 4

  def random[F[_]: Randomiser: Functor]: F[CodePeg] = Randomiser[F].randomInt(Size).map(CodePeg.fromOrdinal)
}
