package model

import effects.Sync
import typeclasses.{ApplicativeOps, Monad, Read, Show, TraverseOps}

// Using tuples screwed up the whole derivation machinery, causing class cast exception (for show). Nasty!
// java.lang.ClassCastException: model.Board$$anon$1 cannot be cast to scala.deriving.Mirror$Product
final case class Board(targetRow: CodeRow, completedRows: List[CompletedRow]) derives Show

final case class CompletedPeg(codePeg: CodePeg, keyPeg: KeyPeg)

type CodeRow = List[CodePeg]
type CompletedRow = List[CompletedPeg]
type RoundResult = List[KeyPeg]

object Board extends TraverseOps with ApplicativeOps {
  def initialise(codePegs: CodePeg*): Board = Board(codePegs.toList, Nil)

  def parseInitialBoard(str: String): Either[String, Board] =
    str.toList.traverse(s => Read[CodePeg].read(s.toString).toRight(s"Invalid code peg $s")).map(initialise)

  def random[F[_]: Sync: Monad](size: Int): F[Board] =
    List.fill(size)(CodePeg.random[F]).sequence.map(xs => initialise(xs: _*))

}


