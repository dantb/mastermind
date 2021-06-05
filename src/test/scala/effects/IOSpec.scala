package effects

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import typeclasses.syntax.monad._

import scala.util.Random

class IOSpec extends ScalaCheckSuite {

  val random = new Random()

  val liftIOGen: Gen[Int => IO[Int]] = Gen.const { (z: Int) =>
    if random.nextInt() % 2 == 0 then IO.Pure(z) else IO.Suspended(() => z)
  }

  given Arbitrary[Int => IO[Int]] = Arbitrary(liftIOGen)

  property("Many sequenced IOs should not overflow the stack") {
    forAll { (liftIO: Int => IO[Int]) =>
      val count = 100000
      val list: List[Int] = List.fill(count)(1)
      val result: IO[Int] = list.foldLeft(liftIO(0)) { (acc, next) =>
        acc.flatMap(prev => liftIO(next + prev))
      }
      assertEquals(IO.runIO(result), count)
    }
  }

}
