package fp.chapter4

/**
 *
 * Created by Ivan Valchev (ivan.valchev@estafet.com) on 1/5/15.
 */

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = if (map(f) getOrElse false) this else None
}

object Option {
  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = oa flatMap (a => ob map (b => f(a, b)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(Nil): Option[List[A]])((oa, as) => oa flatMap (a => as map (a :: _)))
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Chapter4 {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) map (m => xs.map(x => math.pow(x - m, 2))) flatMap mean

  def sequence[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(a => a)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as.foldRight[Option[List[B]]](Some(Nil))((a, b) => Option.map2(f(a), b)(_ :: _))
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => this
    case Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

object Either {
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Partial[+A, +B] {
  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] = this match {
    case Success(b) => f(b)
    case Errors(as) => Errors(as)
  }

  def map2[AA >: A, C, D](b: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] = (this, b) match {
    case (Errors(as), Errors(bs)) => Errors(as ++ bs)
    case (_, Errors(bs)) => Errors(bs)
    case (Errors(as), _) => Errors(as)
    case (Success(aa), Success(bb)) => Success(f(aa, bb))
  }
}

case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]

case class Success[+B](get: B) extends Partial[Nothing, B]
