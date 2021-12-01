package effects

import typeclasses.{ Monad, MonadThrow }

import scala.util.Try

enum IO[A]:
  case Suspended(thunk: () => A) extends IO[A]
  case Pure(a: A) extends IO[A]
  case Error(e: Throwable) extends IO[A]

object IO:
  def runIO[A](io: IO[A]): A =
    io match
      case Suspended(thunk) => thunk()
      case Pure(a) => a
      case Error(e) => throw e

  def fromEither[A](either: Either[Throwable, A]): IO[A] = either match
    case Left(e) => Error(e)
    case Right(a) => Pure(a)

  given Console[IO] = new Console[IO]:
    def read: IO[String] = IO.Suspended(() => scala.io.StdIn.readLine())
    def write(s: String): IO[Unit] = IO.Suspended(() => print(s))

  given Monad[IO] = new Monad[IO]:
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa match
      case Suspended(thunk) => f(thunk())
      case Pure(a) => f(a)
      case Error(e) => Error(e)
    def pure[A](a: A): IO[A] = IO.Pure(a)

  given Sync[IO] = new Sync[IO]:
    def delay[A](a: => A): IO[A] = IO.Suspended(() => a)

  given MonadThrow[IO] = new MonadThrow[IO]:
    def raiseError[A](e: Throwable): IO[A] = IO.Error(e)
    def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = fa match
      case Error(e) => f(e)
      case Suspended(thunk) => Try(Suspended(thunk)).fold(f, identity)
      case Pure(a) => Pure(a)

end IO
