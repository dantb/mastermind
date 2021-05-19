package effects

import typeclasses.Monad

enum IO[A]:
  case Suspended(thunk: () => A) extends IO[A]
  case Pure(a: A) extends IO[A]
  case Error(e: Throwable) extends IO[A]

object IO:
  def runIO[A](io: IO[A]): A =
    io match
      case Suspended(thunk) => thunk()
      case Pure(a)          => a
      case Error(e)       => throw e

  def fromEither[A](either: Either[Throwable, A]): IO[A] = either match {
    case Left(e) => Error(e)
    case Right(a) => Pure(a)
  }

  given Console[IO] = new Console[IO]:
    def read: IO[String] = IO.Suspended(() => scala.io.StdIn.readLine())
    def write(s: String): IO[Unit] = IO.Suspended(() => print(s))

  given Monad[IO] = new Monad[IO]:
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa match {
      case Suspended(thunk) => f(thunk())
      case Pure(a) => f(a)
      case Error(e) => Error(e)
    }
    def pure[A](a: A): IO[A] = IO.Pure(a)

  given Sync[IO] = new Sync[IO]:
    def delay[A](a: => A): IO[A] = IO.Suspended(() => a)

end IO
