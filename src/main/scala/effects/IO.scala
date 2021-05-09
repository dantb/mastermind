package effects

enum IO[A]:
  case Suspended(thunk: () => A) extends IO[A]
  case Pure(a: A) extends IO[A]

object IO:

  given consoleInstance: Console[IO] = new Console[IO]:
    def read: IO[String] = IO.Suspended(() => scala.io.StdIn.readLine())
    def write(s: String): IO[Unit] = IO.Suspended(() => print(s))

  def runIO[A](io: IO[A]): A =
    io match
      case Suspended(thunk) => thunk()
      case Pure(a)          => a

end IO
