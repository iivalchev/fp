package fp


import fp.Chapter8._
import fp.chapter4.Left


/**
 * Created by ivan on 5/27/15.
 */

object Chapter9 {

  trait Parsers[ParseError, Parser[+ _]] {
    self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def char(c: Char): Parser[Char]

    def map[A, B](p: Parser[A])(f: A => B): Parser[B]

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[A]

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

  trait Laws[ParseError, Parser[+ _]] extends App {

    val p: Parsers[ParseError, Parser]

    import p._

    Prop.forAll(Gen.choose(1, 26).map(_.toChar))(c => p.run(p.char(c))(c.toString) == Right(c))

    Prop.forAll(Gen.listOf(Gen.choose(1, 26)).map(xs => xs.map(_.toString).sum))(s => p.run(s)(s) == Right(s))

    Prop.check(p.run("abra" | "cadabra")("abra") == Right("abra"))

    Prop.check(p.run("abra" | "cadabra")("cadabra") == Right("cadabra"))

    Prop.check(p.run(p.listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"))

    Prop.check(p.run(p.listOfN(3, "ab" | "cad"))("abcadcad") == Right("abcadcad"))

    Prop.check(p.run(p.listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))

  }

}
