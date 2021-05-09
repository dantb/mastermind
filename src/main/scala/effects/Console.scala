package effects

trait Console[F[_]]:
  def read: F[String]
  def write(str: String): F[Unit]
  def writeLn(str: String): F[Unit] = write(str + "\n")

trait ConsoleOps:
  extension [F[_]: Console](s: String) def write: F[Unit] = Console[F].write(s)
  extension [F[_]: Console](s: String) def writeLn: F[Unit] = Console[F].writeLn(s)

object Console {
  def apply[F[_]](using c: Console[F]) = c

}
