package typeclasses

import effects.Sync

import scala.compiletime.ops.int.*
import scala.util.Random

trait Randomiser[F[_]]:
  def randomInt[N <: Int, PosInt <: S[N]](range: PosInt): F[Int]

object Randomiser:
  def apply[F[_]: Randomiser]: Randomiser[F] = summon[Randomiser[F]]

  given [F[_]: Sync]: Randomiser[F] = new Randomiser[F]:
    def randomInt[N <: Int, PosInt <: S[N]](range: PosInt) = Sync[F].delay {
      new Random().nextInt(range)
    }

end Randomiser
