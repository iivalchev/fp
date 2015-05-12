package fp

import java.util.concurrent.{ExecutorService, Executors}

import fp.Chapter6.{RNG, State}
import fp.Chapter7.Par

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
    def isFalsified = false
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, success: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop((m, n, rng) => run(m, n, rng) match {
      case Passed | Proved => p.run(m, n, rng)
      case f => f
    })

    def ||(p: Prop): Prop = Prop((m, n, rng) => run(m, n, rng) match {
      case _: Falsified => p.run(m, n, rng)
      case s => s
    })
  }

  object Prop {
    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
      (m, n, rng) => {
        Stream.unfold(rng)(r => Some(as.sample.run(r))).zip(Stream.from(0)).take(n).map {
          case (a, i) => if (f(a)) Passed else Falsified(a.toString, i)
        }.find(_.isFalsified).getOrElse(Passed)
      }
    )

    def forAll[A](as: SGen[A])(f: A => Boolean): Prop = Prop(
      (m, n, rng) => {
        Stream.from(0).take(m).map(i => forAll(as.forSize(i))(f)).toList.reduce(_ && _).run(m, n / m, rng)
      }
    )

    def S: Gen[ExecutorService] =
      Gen.weighted(Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
        Gen.unit(Executors.newSingleThreadExecutor()) -> .25)

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case s ** a => f(a)(s).get }

    def check(p: => Boolean): Prop = Prop((_, _, _) => if (p) Proved else Falsified("()", 0))

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = RNG(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, property proved.")
      }
  }

  case class Gen[+A](sample: State[RNG, A]) {
    self =>

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

    def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(i => Gen(State.sequence(List.fill(i)(sample))))

    def unsized: SGen[A] = SGen(_ => self)

    def **[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
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

    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n max 1, g))
  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))
  }

  Prop.run(Prop.forAll(Gen.listOf1(Gen.choose(-10, 10))) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  })


  Prop.run(Prop.forAll(Gen.listOf1(Gen.choose(-10, 10))) { l =>
    val ls = l.sorted
    ls.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  })
}
