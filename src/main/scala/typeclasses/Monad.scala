package typeclasses

trait Monad[F[_]]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def pure[A](a: A): F[A]
  final def flatten[A](ffa: F[F[A]]): F[A] = flatMap[F[A], A](ffa)(fa => map(fa)(identity))
  final def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

trait MonadOps:
  extension [F[_]: Monad, A, B](fa: F[A])
    def flatMap(f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
    def map(f: A => B): F[B] = Monad[F].map(fa)(f)

  extension [F[_]: Monad, A](a: A)
    def pure(f: A => F[A]): F[A] = Monad[F].pure(a)

  extension [F[_]: Monad, A](ffa: F[F[A]])
    def flatten: F[A] = Monad[F].flatten(ffa)

object Monad:
  def apply[F[_]](using m: Monad[F]): Monad[F] = m

end Monad
