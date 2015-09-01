package fp

import fp.Chapter12.Applicative
import fp.Chapter6.State
import fp.Chapter7.Par
import fp.Chapter8.Gen
import fp.Chapter9.Parsers

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 8/3/15.
 */
object Chapter11 extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Applicative[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(a => unit(f(a)))
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = flatMap(fa)(a => map(fb)(b => f(a, b)))
    def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((fa, b) => flatMap(fa)(a => map(b)(a :: _)))
    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms.foldRight(unit(List[A]()))((a, l) => flatMap(f(a))(b => if (b) map(l)(a :: _) else l))
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)


    def flatMap_compose[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())
    def flatMap_join[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

    def compose_join[A, B, C](fa: A => F[B], fb: B => F[C]): A => F[C] = a => join(map(fa(a))(fb))
  }

  object Monad {
    def genMonad = new Monad[Gen] {
      override def unit[A](a: => A): Gen[A] = Gen.unit(a)
      override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa.flatMap(f)
    }

    def parserMonad[P[+ _]](p: Parsers[P]) = new Monad[P] {
      override def unit[A](a: => A): P[A] = p.succeeded(a)
      override def flatMap[A, B](fa: P[A])(f: (A) => P[B]): P[B] = p.flatMap(fa)(f)
    }

    def parMonad = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)
      override def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(fa)(f)
    }

    def optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Option(a)
      override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa.flatMap(f)
    }

    def streamMonad = new Monad[Stream] {
      override def unit[A](a: => A): Stream[A] = Stream(a)
      override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa.flatMap(f)
    }

    def listMonad = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)
      override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa.flatMap(f)
    }

    def stateMonad[S] = new Monad[({type T[+A] = State[S, A]})#T] {
      override def unit[A](a: => A): State[S, A] = State.unit(a)
      override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa.flatMap(f)
    }

    def idMonad = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)
      override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = fa.flatMap(f)
    }

    def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }
  }


  println(Monad.listMonad.sequence(List(List(1, 2), List(3, 4))))

  println(Monad.listMonad.sequence(List(List(3, 4))))

  println(List(1, 2, 3).flatMap(List(_)))

  val l = List.fill(3)(List(1, 2))
  println(l)
  println(Monad.listMonad.sequence(l))

}
