package typeclasses

trait MonadError[F[_]: Monad, E]:
  final def monad: Monad[F] = Monad[F]
  final def handleError[A](fa: F[A])(f: E => A): F[A] =
    handleErrorWith(fa)(e => Monad[F].pure(f(e)))
  def raiseError[A](e: E): F[A]
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

trait MonadErrorOps:
  extension [E, A, F[_]](fa: F[A])
    def handleErrorWith(f: E => F[A])(using MonadError[F, E]): F[A] =
      MonadError[F, E].handleErrorWith(fa)(f)
    def handleError(f: E => A)(using MonadError[F, E]): F[A] = MonadError[F, E].handleError(fa)(f)

object MonadError:
  def apply[F[_], E](using m: MonadError[F, E]): MonadError[F, E] = m

end MonadError

type MonadThrow[F[_]] = MonadError[F, Throwable]

object MonadThrow:
  def apply[F[_]](using mt: MonadThrow[F]) = mt

end MonadThrow
