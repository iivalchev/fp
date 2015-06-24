package fp

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
  def concatenate[A](as: List[A])(m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
  }
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }
//  def foldLeft[A, B](as: List[A], zero: B)(f: (B, A) => B): B = {
//    foldMap(as, )
//  }

  val words = List("Hic", "Est", "Index")
  println(words.foldRight(stringMonoid.zero)(stringMonoid.op))
  println(words.foldLeft(stringMonoid.zero)(stringMonoid.op))

}
