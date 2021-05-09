package effects

import typeclasses.Monad

enum IO[A]:
  case Suspended(thunk: () => A) extends IO[A]
  case Pure(a: A) extends IO[A]

object IO:
  def runIO[A](io: IO[A]): A =
    io match
      case Suspended(thunk) => thunk()
      case Pure(a)          => a

  given Console[IO] = new Console[IO]:
    def read: IO[String] = IO.Suspended(() => scala.io.StdIn.readLine())
    def write(s: String): IO[Unit] = IO.Suspended(() => print(s))

  given Monad[IO] = new Monad[IO]:
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa match {
      case Suspended(thunk) =>
        val newThunk: () => B = () => {
          f(thunk()) match {
            case Suspended(thunk0) => thunk0()
            case Pure(b) => b
          }
        }
        Suspended(newThunk)
      case Pure(a) => f(a)
    }
    def pure[A](a: A): IO[A] = IO.Pure(a)


end IO
