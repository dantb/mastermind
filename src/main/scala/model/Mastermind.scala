package model

import typeclasses.{Read, Show, TraverseOps}

object Mastermind extends TraverseOps {
  // Using tuples screwed up the whole derivation machinery, causing class cast exception (for show). Nasty!
  // java.lang.ClassCastException: model.Board$$anon$1 cannot be cast to scala.deriving.Mirror$Product
  final case class Board(targetRow: List[CodePeg], completedRows: List[CompletedRow]) derives Show

  object Board {
    def initialise(codePegs: CodePeg*): Board = Board(codePegs.toList, Nil)

    def parseInitialBoard(str: String): Either[String, Board] =
      str.toList.traverse(s => Read[CodePeg].read(s.toString).toRight(s"Invalid code peg $s")).map(initialise)
  }

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
  }

  enum KeyPeg derives Show:
    case White, Black, Missed

  final case class CompletedPeg(codePeg: CodePeg, keyPeg: KeyPeg)

  type CodeRow = List[CodePeg]
  type CompletedRow = List[CompletedPeg]

//  def insertPeg(board: Board, thing: (CodePeg, KeyPeg)) = board.copy(rows = List(thing))

}
