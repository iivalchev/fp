package fp


import fp.Chapter8._


/**
 * Created by ivan on 5/27/15.
 */

object Chapter9 {

  trait Parsers[ParseError, Parser[+ _]] {
    self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def succeeded[A](a: A): Parser[A] = string("").map(_ => a)

    def failed[A]: Parser[A]

    def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

    def map[A, B](p: Parser[A])(f: A => B): Parser[B]

    def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = product(p1, p2).map { case (a, b) => f(a, b) }

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]

    implicit def parserOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    def many[A](a: Parser[A]): Parser[List[A]]

    def many1[A](p: Parser[A])(notFound: ParseError) = map2(p, p.many)

    def slice[A](p: Parser[A]): Parser[String]

    def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]) = self.or(p, p2)

      def or[B >: A](p2: Parser[B]) = self.or(p, p2)

      def map[B](f: A => B) = self.map(p)(f)

      def many: Parser[List[A]] = self.many(p)

      def slice: Parser[String] = self.slice(p)

      def **[B](p2: Parser[B]) = self.product(p, p2)

      def product[B](p2: Parser[B]) = self.product(p, p2)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(s: String) = run(p1)(s) == run(p2)(s)

      def succeededLaw[A](a: A, g: Gen[String]) = Prop.forAll(g)(in => run(succeeded(a))(in) == a)

      def charLaw(g: Gen[Char]) = Prop.forAll(g)(c => run(char(c))(c.toString) == Right(c))

      def stringLaw(g: Gen[String]) = Prop.forAll(g)(s => run(s)(s) == Right(s))

      def orLaw = Prop.check(run("abra" | "cadabra")("abra") == Right("abra")) &&
        Prop.check(run("abra" | "cadabra")("cadabra") == Right("cadabra"))

      def listOfNLaw = Prop.check(run(self.listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")) &&
        Prop.check(run(self.listOfN(3, "ab" | "cad"))("abcadcad") == Right("abcadcad")) &&
        Prop.check(run(self.listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))

      def mapLaw[A](p: Parser[A])(g: Gen[String]) = Prop.forAll(g)(in => equal(p, p.map(a => a))(in))

      def productLaw = Prop.check(run(string("a") product string("b"))("ab") == Right(("a", "b"))) &&
        Prop.check {
          run(failed product succeeded(""))("") match {
            case _: Left => true
            case _ => false
          }
        } &&
        Prop.check {
          run(succeeded("") product failed)("b") match {
            case _: Left => true
            case _ => false
          }
        }
    }

  }

}
