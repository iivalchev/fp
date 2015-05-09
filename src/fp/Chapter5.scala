package fp

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 1/24/15.
 */

sealed trait Stream[+A] {
  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, bs) => {
    lazy val b = f(a)
    Cons(() => b, () => bs)
  })

  def filter(p: A => Boolean) = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  def add[B >: A](el: B) = foldRight(Empty: Stream[B])((a, b) => Cons(() => a,
    () => b match {
      case Empty => Cons(() => el, () => Empty)
      case _ => b
    }))

  def append[B >: A](as: Stream[B]) = foldRight(Empty: Stream[B])(
    (a, b) => Cons(() => a, () => b match {
      case Empty => as
      case _ => b
    }))

  def flatMap[B](f: A => Stream[B]) = foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  def zip[B](bs: Stream[B]): Stream[(A, B)] = Stream.unfold((this, bs)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](bs: Stream[B]) = Stream.unfold((this, bs)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Empty))
    case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def startsWith[A1 >: A](s: Stream[A1]): Boolean = zipAll(s) forAll {
    case (Some(a1), Some(a2)) if a1 == a2 => true
    case (Some(_), None) => true
    case _ => false
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case s@Cons(_, t) => Some(s, t())
    case _ => None
  }

  def exists(p: A => Boolean) = foldRight(false)(p(_) || _)

  def hasSubsequence[A1 >: A](s: Stream[A1]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(Stream.cons(z, Empty))((a, b) => {
    lazy val b1 = b
    Stream.cons(f(a, b1.hd()), b1)
  })

  def find(p: A => Boolean): Option[A] = foldRight(None: Option[A])((a, oa) => if (p(a)) Some(a) else oa)

}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]) = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A] = Empty

  def apply[A](a: A*): Stream[A] = if (a.isEmpty) empty else cons(a.head, apply(a.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs0: Stream[Int] = {
    def fib(n: Int): Int = n match {
      case x if x < 0 => 0
      case 0 => 0
      case 1 => 1
      case x => fib(x - 1) + fib(x - 2)
    }
    def f(n: Int): Stream[Int] = Stream.cons(fib(n), f(n + 1))
    f(0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map(t => Stream.cons(t._1, unfold(t._2)(f))).getOrElse(Empty)

  def map[A, B](as: Stream[A])(f: A => B) = unfold(as) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def take[A](as: Stream[A])(n: Int) = unfold(as) {
    case Cons(h, t) if n > 0 => Some(h(), t())
    case _ => None
  }

  def takeWhile[A](as: Stream[A])(p: A => Boolean) = unfold(as) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zip[A, B](as: Stream[A], bs: Stream[B]) = unfold((as, bs)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[A, B](as: Stream[A], bs: Stream[B]) = unfold((as, bs)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Empty))
    case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def fibs: Stream[Int] = Stream.cons(0, Stream.cons(1, unfold((0, 1))(t => Some(t._1 + t._2, (t._2, t._1 + t._2)))))
}

case class Cons[+A](hd: () => A, tl: () => Stream[A]) extends Stream[A]

case object Empty extends Stream[Nothing]

object Chapter5 {

}
