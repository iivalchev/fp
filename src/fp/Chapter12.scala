package fp

import fp.Chapter10.{Foldable, Monoid}
import fp.Chapter11.{Functor, Monad}
import fp.Chapter6.State
import fp.Chapter6.State.{get, set}

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 8/12/15.
 */
object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] {
    self =>
    def unit[A](a: => A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit[A => B => C](f.curried))(fa))(fb)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

    def map_apply[A, B](fa: F[A])(f: A => B): F[B] = apply(unit[A => B](f))(fa)

    def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
    def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(fa => fa)
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit[A => B => C => D](f.curried))(fa))(fb))(fc)
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(apply(unit[A => B => C => D => E](f.curried))(fa))(fb))(fc))(fd)

    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(f: (F[A], G[A])): (F[B], G[B]) = (self.apply(fs._1)(f._1), G.apply(fs._2)(f._2))
    }

    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(fga, fgb)(G.map2(_, _)(f))
    }

    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.keySet.foldLeft(unit(Map[K, V]()))((fm, k) => map2(fm, ofa(k))(_.updated(k, _)))

  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  object Applicative {
    def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (f@Failure(_, _), _) => f
        case (_, f@Failure(_, _)) => f
      }
    }
  }

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
    self =>

    import Traverse._

    def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
    def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = {
      type Id[A] = A
      implicit val idApplicative = new Applicative[Id] {
        def unit[A](a: => A): Id[A] = a
        override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] = fab(fa)
      }
      traverse[Id, A, B](fa)(f)
    }
    def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B = traverse(as)(f)(monoidApplicative(m))
    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)
    def zipWithIndex[A](fa: F[A]): F[(A, Int)] = traverseS(fa)(
      a => for {
        i <- get[Int]
        _ <- set(i + 1)
      } yield (a, i)).run(0)._1
    override def toList[A](fa: F[A]): List[A] = traverseS(fa)(
      a => for {
        as <- get[List[A]]
        _ <- set(a :: as)
      } yield ()).run(List[A]())._2
    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa)(
      a => for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield b).run(s)
    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => (G[B], H[B]))(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
      traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(f)(G product H)
    def compose[G[_]](G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
      new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[M[_]:Applicative, A, B](fa:F[G[A]])(f: A => M[B]) =
          self.traverse[M, G[A], G[B]](fa)(ga => G.traverse(ga)(f))
      }
    }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  object Traverse {
    def listTraverse = new Traverse[List] {
      override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        fa.foldRight(G.unit(List[B]()))((a, fb) => G.map2(f(a), fb)(_ :: _))
    }

    def treeTraverse = new Traverse[Tree] {
      override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = Tree(f(fa.head), fa.tail.map(map(_)(f)))
      override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
        G.map2(f(fa.head), listTraverse.traverse(fa.tail)(traverse(_)(f)))(Tree(_, _))
      }
    }

    def optionTraverse = new Traverse[Option] {
      override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa.map(f)
      override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
        fa match {
          case Some(a) => G.map(f(a))(Some(_))
          case None => G.unit(None)
        }
    }

    type Const[M, A] = M

    implicit def monoidApplicative[M](implicit M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

    def compose[F[_], G[_]](F:Monad[F], G:Monad[G], T:Traverse[G]):Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def flatMap[A, B](fa: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] = join(map(fa)(f))
      override def join[A](fa:F[G[F[G[A]]]]):F[G[A]] = F.map(F.flatMap[G[F[G[A]]], G[G[A]]](fa)(gfg => T.sequence(gfg)))(G.join(_))
    }
  }

}
