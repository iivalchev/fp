package fp

import fp.Chapter6.{RNG, State}

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 4/8/15.
 */
object Chapter8 extends App {

  type FailedCase = String

  type SuccessCount = Int

  type TestCases = Int

  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Failed(failure: FailedCase, success: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop((m, n, rng) => run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case f => f
    })

    def ||(p: Prop): Prop = Prop((m, n, rng) => run(m, n, rng) match {
      case _: Failed => p.run(m, n, rng)
      case s => s
    })
  }

  object Prop {
    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
      (m, n, rng) => {
        Stream.unfold(rng)(r => Some(as.sample.run(r))).zip(Stream.from(0)).take(n).map {
          case (a, i) => if (f(a)) Passed else Failed(a.toString, i)
        }.find(_.isFalsified).getOrElse(Passed)
      }
    )

    def forAll[A](as: SGen[A])(f: A => Boolean): Prop = Prop(
      (m, n, rng) => {
        Stream.from(1).take(m).map(i => forAll(as.forSize(i))(f)).toList.reduce(_ && _).run(m, n / m, rng)
      }
    )

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = RNG(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Failed(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
      }

  }

  case class Gen[+A](sample: State[RNG, A]) {
    self =>

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(i => Gen(State.sequence(List.fill(i)(sample))))

    def unsized: SGen[A] = SGen(_ => self)
  }

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(rng => {
      val (n, rng1) = RNG.nonNegativeInt(rng)
      (start + n % (stopExclusive - start), rng1)
    }))

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State[RNG, Int](rng => rng.nextInt).map(_ % 2 == 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
      Gen(State(RNG.double)).flatMap(d => if (d < threshold) g1._1 else g2._1)
    }

    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen(State.sequence(List.fill(n)(g.sample))))
  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))
  }

  val smallInt = Gen.choose(1, 10)
  val maxProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  Prop.run(maxProp)
}
