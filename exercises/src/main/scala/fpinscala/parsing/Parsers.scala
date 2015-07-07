package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop
import scala.language.higherKinds
import scala.language.implicitConversions
import fpinscala.monads.Monad



// object Parser {
//   type Parser[+A] = Location => Result[A]
// }

/*
 All Parsers in this example take a String as input.  The parametric type A is the type "measured": a count, a string, a char.
 Parser[Int] that counts the number of chars "x" will require a Parser[Char] to function.
 */
// parametric type ParseError no longer needed -- made concrete
//trait Parsers[ParseError, Parser[+_]] { self => // so

// Keep parametric type Parser in Parsers signature -- part of lesson
trait Parsers[Parser[+_]] { self => // so inner inner classes may call methods of trait
  implicit def string(s: String): Parser[String]
  // not an implicit convesion String => Regex?
  // Regex => Parser[String]??
  //implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
    implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  //val parserMonad = Monad.parserMonad[Parser](self)

  // trait Result[+A]
  // case class Success[+A](get: A, charsConsumed: Int) extends
  //     Result[A]
  // case class Failure(get: ParseError) extends
  //     Result[Nothing]
  // from 9.6.2

  import Parsers.Result
  def run[A](p: Parser[A])(input: String): Result[A]
  def flatMap[A,B](p: Parser[A])(f: A=>Parser[B]): Parser[B]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def map[A,B](p: Parser[A])(f: A=>B): Parser[B] = {
    // verify that 'succeed' serves as 'unit'
    val g: A => Parser[B] = (a: A) => succeed(f(a))
    p.flatMap(g)
  }

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p.flatMap((a: A) => {
      p2.map((b: B) => (a,b))
    }
    )

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B)=>C): Parser[C] = {
    val parserAB: Parser[(A,B)] = p.product(p2)
    val parserC: Parser[C] = parserAB.map(
      (tup: (A,B)) => f(tup._1, tup._2)
    )
    parserC
  }
  // akin to unit?
  def succeed[A](a: A): Parser[A] =
    string("").map((s: String) => a)


  // parser returned recognized char c
  def char(c: Char): Parser[Char] =
    string(c.toString).map((s: String)=>s.charAt(0))

  // parser returned recognizes string s
  //def string(s: String): Parser[String]

  // what does this do?
  def many[A](p: Parser[A]): Parser[List[A]] = {
    val ll: Parser[List[A]] = map2(p, many(p))(
      (a: A, la: List[A]) => a::la)
    val empty: Parser[List[A]] = succeed(List[A]())

    val combined: Parser[List[A]] = ll.or(empty)
    combined
  }


  /*
With many1 , we can now implement the parser for zero or more 'a' followed by one or more 'b' as follows:
char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
   */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p,many(p))((a: A, la: List[A]) => a::la)
  // section 9.2.1 infers many1 is to be implemented with product
//    product(p, many(p))

  // "see what portion of the input string" is examined
  def slice[A](p: Parser[A]): Parser[String]

  // parser returned recognizes either p1 or p2
  /*
  run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")
   */

  // use map2 and succeed
  /*
run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab","ab","cad"))
run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad","ab","ab"))
run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab","ab","ab"))
   */

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if(n<=1){
      p.map((a: A)=>List(a))
    } else {
      val pla: Parser[List[A]] = listOfN(n-1, p)
      p.map2(pla)((a: A, la: List[A]) => a::la)
    }
  }


  // [error]  Note: implicit method operators is not applicable here because it comes after the application point and it lacks an explicit result type
  // implicit def string(s: String): Parser[String]
  // implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  // implicit def asStringParser[A](a: A)(
  //   implicit f: A => Parser[String]): ParserOps[String] =
  //   ParserOps(f(a))
  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A=>B): Parser[B] = self.map(p)(f)
    def map2[B,C](p2: Parser[B])(f: (A,B)=>C): Parser[C] =
      self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)
    def product[B](p2: Parser[B]): Parser[(A,B)] =
      self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = this.product(p2)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def |[B>:A](p2: Parser[B]): Parser[B] = this.or(p2)

    // We expect that, for instance,
    // run(numA)("aaa") gives Right(3) ,
    // and run(numA)("b") gives Right(0) .
    // val numberOfA: Parser[Int] = char('a').many.map(
    //   (s: String) => s.size)
    val numberOfA: Parser[Int] = char('a').many.slice.map(
      (s: String) => s.size)

  }

  object Laws {
    //run(char(c))(c.toString) == Right(c)
    // map(p)(a => a) == p

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)((s: String) => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    // run(succeed(a))(s) == Right(s)
    def succeedLaw[A](genString: Gen[String], genA: Gen[A]): Prop = {
      val genStringAndA: Gen[(String, A)] =
        genString.**(genA)
      Prop.forAll(genStringAndA)((tup: (String, A)) => {
        val string: String = tup._1
        val a: A = tup._2
        val sucA: Parser[A] = succeed(a)
        run(sucA)(string) == Right(a)
      }
      )
    }


    // check the behavior of product ---> Monad Laws
    // def productLaw[A,B](pa: Parser[A], pb: Parser[B])(
    //   in: Gen[String]): Prop = {
    //   val pab: Parser[(A,B)] = pa.product(pb)
    //   /*
    //    What needs to be shown for Parser[(A,B)] 
    //    and Parser[A], Parser[B]?

       
    //    */
    // }

  }
}

object Parsers {

  // section 9.6.1
  // replaced by section 9.6.2
  // type Parser[+A] = String => Either[ParseError, A]
  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends
      Result[A]
  case class Failure(get: ParseError) extends
      Result[Nothing]
  type LocationResultParser[+A] = Location => Result[A]


  object SimpleParser extends Parsers[LocationResultParser]{

    implicit def string(s: String): LocationResultParser[String] =
      (in: Location) => {
        val strIn: String = in.currentLine
        if(strIn.startsWith(s))
          Success(strIn, strIn.length) // why is strIn.length necessary?
        else Failure(
          ParseError(
            List((in, strIn))
          )
        )
      }

    implicit def regex(r: Regex): LocationResultParser[String] =
      string(r.regex)  // sure???

    def run[A](p: LocationResultParser[A])(input: String): Result[A] =
      p(Location(input))

    def flatMap[A,B](p: LocationResultParser[A])(f: A=>LocationResultParser[B]): LocationResultParser[B] =
      (locIn: Location) => {
        val resultA: Result[A] = p(locIn)
        //val resultB: Result[B] = f(resultA)
        // not Result[B]! Parser[B].
        val parserB: LocationResultParser[B] = resultA match {
          case Success(a: A, charsConsumed: Int) => f(a)
          case Failure(err: ParseError) =>
            (failLoc: Location) => Failure(err: ParseError)
        }
        parserB(locIn)  // sure the same Location is used twice?
      }

    def or[A](p1: LocationResultParser[A], p2: => LocationResultParser[A]): LocationResultParser[A] =
    (locIn: Location) => {
      val result1: Result[A] = p1(locIn)
      lazy val result2: Result[A] = p2(locIn)
      result1 match {
        case suc1: Success[A] => suc1
        case Failure(err1: ParseError) => result2 match {
          case suc2: Success[A] => suc2
          case Failure(err2: ParseError) => {
            val combinedErr: ParseError =
              ParseError(err1.stack,
                err2 :: err1.otherFailures)
            val combinedFailure: Failure = Failure(combinedErr)
            combinedFailure
          }
        }
      }
    }

  }
}




case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
