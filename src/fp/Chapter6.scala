/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 2/5/15.
 */
package fp

object Chapter6 {

  case class RNG(seed: Long) {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = RNG(newSeed)
      val nextInt = (newSeed >>> 16).toInt
      (nextInt, nextRNG)
    }
  }

  object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    def double(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)


    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d4, r3) = double(r2)
      ((d1, d2, d4), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count <= 0) {
        (Nil, rng)
      } else {
        val (i, r1) = rng.nextInt
        val (l, r2) = ints(count - 1)(r1)
        (i :: l, r2)
      }
    }

    def unit[A](a: A): Rand[A] = (a, _)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
      flatMap(s)(a => unit(f(a)))
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((ra, rl) => map2(ra, rl)(_ :: _))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
      r0 => {
        val (a, r1) = f(r0)
        g(a)(r1)
      }
    }
  }

  import fp.Chapter6.State._

  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(s0 => {
        val (a, s1) = run(s0)
        f(a).run(s1)
      })
    }

    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => s.map(b => f(a, b)))
    }
  }

  object State {
    def unit[S, A](a: => A): State[S, A] = State((a, _))

    def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] = as.foldRight(unit[S, List[A]](Nil))((s, bs) => s.map2(bs)(_ :: _))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }


  type Rand[+A] = RNG => (A, RNG)

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs.map(i => modify[Machine](m => (i, m) match {
        case _ => m
      })))
      m <- get
    } yield (m.coins, m.candies)
  }
}