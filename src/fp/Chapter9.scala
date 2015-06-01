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

    def many[A](a: A): Parser[List[A]]

    def many1[A](a: A)(notFound: ParseError) = map(many(a))(i => if (i.isEmpty) Left(notFound) else Right(i))

    def then[A, B](a: A, b: B): Parser[(Int, Int)]

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]) = self.or(p, p2)

      def or[B >: A](p2: Parser[B]) = self.or(p, p2)

      def map[B](f: A => B) = self.map(p)(f)

      def many(a: A): Parser[List[A]] = self.many(a)
    }

    object Laws {
      def char(g: Gen[Char]) = Prop.forAll(g)(c => run(self.char(c))(c.toString) == Right(c))

      def string(g: Gen[String]) = Prop.forAll(g)(s => run(s)(s) == Right(s))

      def or = Prop.check(run("abra" | "cadabra")("abra") == Right("abra")) &&
        Prop.check(run("abra" | "cadabra")("cadabra") == Right("cadabra"))

      def listOfN = Prop.check(run(self.listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")) &&
        Prop.check(run(self.listOfN(3, "ab" | "cad"))("abcadcad") == Right("abcadcad")) &&
        Prop.check(run(self.listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))
    }
  }
}
