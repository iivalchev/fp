package fp


/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 9/10/15.
 */
object Chapter13 extends App {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  object Free {
    def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return(a)
      override def flatMap[A, B](fa: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = fa flatMap f
    }

    @annotation.tailrec
    def runTrampoline[A](a: Free[Function0, A]): A = {
      a match {
        case Return(x) => x
        case Suspend(r) => r()
        case FlatMap(s, f) => s match {
          case Return(x1) => runTrampoline(f(x1))
          case Suspend(r) => runTrampoline(f(r()))
          case FlatMap(s1, g) => runTrampoline(s1 flatMap (g(_) flatMap f))
        }
      }
    }

    def run[F[_], G[_], A](a: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = {
      @annotation.tailrec
      def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
        case FlatMap(FlatMap(s, g), f) => step(s flatMap (g(_) flatMap f))
        case FlatMap(Return(x1), f) => step(f(x1))
        case _ => a
      }
      step(a) match {
        case Return(x) => G.unit(x)
        case Suspend(r) => t(r)
        case FlatMap(s, f) => s match {
          case Suspend(r1) => G.flatMap(t(r1))(a1 => run(f(a1))(t))
          case _ => sys.error("step eliminates this case")
        }
      }
    }

    trait Translate[F[_], G[_]] {
      def apply[A](f: F[A]): G[A]
    }

    type ~>[F[_], G[_]] = Translate[F, G]


    def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
      val t = new ~>[F, ({type f[x] = Free[G, x]})#f] {
        def apply[A](f: F[A]): Free[G, A] = {
          Suspend[G, A](fg[A](f))
        }
      }
      run[F, ({type f[x] = Free[G, x]})#f, A](f)(t)(freeMonad[G])
    }
    //    def runConsole[A](a: Free[Console,A]): A

  }

  trait Monad[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def forever[A](fa: F[A]): F[A] = flatMap(fa)(_ => forever(fa))
  }

  def fun0Monad: Monad[Function0] = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](fa: () => A)(f: (A) => () => B): () => B = () => f(fa())()
  }

  Free.freeMonad.forever(Suspend(() => {
    println("Forever and ever")
  }))
}