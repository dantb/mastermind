package typeclasses

trait Applicative[F[_]]:
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  def pure[A](a: A): F[A]
  final def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    val fab: F[A => (A, B)] = map(fb)(b => (a => (a, b)))
    ap(fa)(fab)
  }
  final def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

trait ApplicativeOps:
  extension [F[_]: Applicative, A](fa: F[A])
    def map[B](f: A => B): F[B] = Applicative[F].map(fa)(f)
    def product[B](fb: F[B]): F[(A, B)] = Applicative[F].product(fa, fb)

  extension [F[_]: Applicative, A](a: A)
    def pure: F[A] = Applicative[F].pure(a)

object Applicative extends MonadOps:
  def apply[F[_]](using a: Applicative[F]): Applicative[F] = a

  given [F[_]: Monad]: Applicative[F] = new Applicative[F]:
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = fa.flatMap(a => f.map(fab => fab(a)))
    def pure[A](a: A): F[A] = Monad[F].pure(a)

  given Applicative[Option] = new Applicative[Option]:
    def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = (fa, f) match {
      case (Some(a), Some(f)) => Option(f(a))
      case _ => None
    }
    def pure[A](a: A): Option[A] = Option(a)


  given [E]: Applicative[[T] =>> Either[E, T]] = new Applicative[[T] =>> Either[E, T]]:
    def ap[A, B](fa: Either[E, A])(f: Either[E, A => B]): Either[E, B] = (fa, f) match {
      case (Right(a), Right(f)) => Right(f(a))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }
    def pure[A](a: A): Either[E, A] = Right(a)

end Applicative
