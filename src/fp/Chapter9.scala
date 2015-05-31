package fp

import fp.chapter4.Left

/**
 * Created by ivan on 5/27/15.
 */

trait Parsers[ParseError, Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def char(c: Char): Parser[Char]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def parserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def many[A](a: A): Parser[Int]

  def many1[A](a: A)(notFound: ParseError) = map(many(a))(i => if (i == 0) Left(notFound) else Some(i))

  def then[A, B](a: A, b: B): Parser[(Int, Int)]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]) = self.or(p, p2)

    def or[B >: A](p2: Parser[B]) = self.or(p, p2)
  }

}

object Chapter9 {

}
