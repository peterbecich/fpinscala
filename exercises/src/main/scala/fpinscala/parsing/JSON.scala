package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._
import scala.language.higherKinds
import scala.language.implicitConversions
import fpinscala.monads.Monad


trait JSON
object JSON {
  /*
object
    {}
    { members }
members
    pair
    pair , members
pair
    string : value
array
    []
    [ elements ]
elements
    value
    value , elements
value
    string
    number
    object
    array
    true
    false
    null

   */
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Bool) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  /*
   These primitives of Parser[JSON] must be implemented:

   string(s) : Recognizes and returns a single String

   regex(s) : Recognizes a regular expression s

   slice(p) : Returns the portion of input inspected by p if successful

   succeed(a) : Always succeeds with the value a

   flatMap(p)(f) : Runs a parser, then uses its result to select a second parser to run in sequence

   or(p1,p2) : Chooses between two parsers, first attempting p1 , and then p2 if p1 fails

   */

  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]):
      Parser[JSON] = {
    import P._

    
  }


  // note input is still string
  // result of parser will be type JSON -- Parser[JSON] needed
  val oracle: String = """
    "Name" : "Oracle",
    "Ticker" : "ORCL",
    "Price" : 15.01,
    "Shares outstanding" : 1.234e5
    "Related companies" : 
      [ "IBM", "YHOO", "MSFT", "GOOG" ]
  """.stripMargin.replaceAll("\n", " ")

  def main(args: Array[String]): Unit = {
    println(oracle)

  }

}
