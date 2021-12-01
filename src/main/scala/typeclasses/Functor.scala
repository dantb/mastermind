package typeclasses

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

trait FunctorOps:
  extension [F[_], A](fa: F[A])
    def map[B](f: A => B)(using Functor[F]): F[B] = Functor[F].map(fa)(f)

object Functor:
  def apply[F[_]](using a: Functor[F]): Functor[F] = a

  given [F[_]: Applicative]: Functor[F] = new Functor[F]:
    def map[A, B](fa: F[A])(f: A => B): F[B] = Applicative[F].map(fa)(f)

end Functor
