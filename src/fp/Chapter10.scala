package fp

import fp.Chapter7.Par

import scala.Predef.augmentString

/**
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 6/24/15.
 */
object Chapter10 extends App {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  class MonoidLaws[A] {
    def opLaw(a1: A, a2: A, a3: A)(m: Monoid[A]) = m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a3, a1))
    def zeroLaw(a: A)(m: Monoid[A]) = m.op(m.zero, a) == a
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    def zero: (A) => A = a => a
  }
  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero: A = m.zero
  }
  def concatenate[A](as: List[A])(m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
  }
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }
  def foldLeft[A, B](as: List[A], zero: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(zero)
  }
  def foldRight[A, B](as: List[A], zero: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(zero)
  }
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.size == 1) f(v.head)
    else {
      val (l, r) = v.splitAt(v.size / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }

  //  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
  //    Par.flatMap(Par.parMap(v)(f))(bs => foldMapV(bs, par(m))(b => Par.unit(b)))
  //  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
    def zero: WC = Stub("")
  }

//  def count(s: String): Int = {
//    def unstub(s: String) = s.length min 1
//    foldMapV(s.toIndexedSeq, wcMonoid)(c => {
//      if (c.isWhitespace) {
//        Part("", 0, "")
//      } else {
//        Stub(c.toString)
//      }
//    }) match {
//      case Stub(s) => unstub(s)
//      case Part(l, w, r) => unstub(l) + w + unstub(r)
//    }
//  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B = sys.error("not implemented")
    def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B = sys.error("not implemented")
    def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]) = foldLeft(as, m.zero)(m.op)
    def toList[A](fa: F[A]): List[A] = foldRight(fa, List(): List[A])(_ :: _)
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B = concatenate(as.map(f))(m)
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A], z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l, foldRight(r, z)(f))(f)
    }
    override def foldLeft[A, B](as: Tree[A], z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r, foldLeft(l, z)(f))(f)
    }
    def foldMap[A, B](as: Tree[A])(f: (A) => B)(m: Monoid[B]): B = as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))
    }
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A], z: B)(f: (A, B) => B): B = as.map(a => f(a, z)).getOrElse(z)
    override def foldLeft[A, B](as: Option[A], z: B)(f: (B, A) => B): B = as.map(a => f(z, a)).getOrElse(z)
    def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B = as.map(f).getOrElse(m.zero)
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B): A => B = a => B.op(f1(a), f2(a))
    def zero: A => B = _ => B.zero
  }

//  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
//    def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keySet ++ a2.keySet).foldLeft(zero)(
//      (acc, k) => acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero))))
//    def zero: Map[K, V] = Map[K, V]()
//  }

//  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

//  val words = List("Hic", "Est", "Index")
//  println(words.foldRight(stringMonoid.zero)(stringMonoid.op))
//  println(words.foldLeft(stringMonoid.zero)(stringMonoid.op))

}
