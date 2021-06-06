package state

import effects.Console
import model._
import typeclasses.syntax.monad._
import typeclasses.{Functor, Monad, Show}

final case class StateT[F[_], S, A](
    run: S => F[(S, A)]
) {
  def get(using Functor[F]): StateT[F, S, S] = StateT(s0 => Functor[F].map(run(s0))((s1, _) => (s1, s1)))
}

object StateT {

  def pure[F[_]: Monad, S, A](a: A): StateT[F, S, A] = StateT(s => Monad[F].pure((s, a)))

  enum GameState:
    case Start, InProgress, End

  given [F[_]: Monad, S]: Monad[[A] =>> StateT[F, S, A]] = new Monad[[A] =>> StateT[F, S, A]]:
    def flatMap[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] =
      StateT(s0 => Monad[F].flatMap(fa.run(s0))((s1, a) => f(a).run(s1)))
    def pure[A](a: A): StateT[F, S, A] = StateT.pure(a)

  def game[F[_]: Monad: Console, Size <: Int]: StateT[F, Board[Size], GameState] =
    for {
        _ <- StateT.pure(())
        gameState <- loop[F, Size]
        outcome <- gameState match {
          case GameState.Start => game[F, Size]
          case GameState.InProgress => game[F, Size]
          case GameState.End => StateT.pure(gameState)
        }
    } yield outcome

  def loop[F[_]: Monad: Console, Size <: Int]: StateT[F, Board[Size], GameState] =
    StateT(s =>
      for {
        codeRow <- readAttempt[F, Size]
        state <- isGameOver[Size](s.copy(completedRows = addKeyPegs[Size](codeRow) :: s.completedRows)) match {
          case true => GameState.End.pure[F]
          case false =>  showBoard[F, Size](s).as[GameState](GameState.InProgress)
        }
      } yield (s, state)
    )

  def readAttempt[F[_]: Console, Size <: Int]: F[CodeRow[Size]] = ???

  def isGameOver[Size <: Int](board: Board[Size]): Boolean = false

  def addKeyPegs[Size <: Int](codeRow: CodeRow[Size]): CompletedRow[Size] = ???

  def showBoard[F[_]: Console, Size <: Int](board: Board[Size]): F[Unit] =
    Console[F].writeLn(Show[Board[Size]].show(board))

}
