package fpinscala.answers.parsing
import scala.language.implicitConversions
import scala.language.higherKinds


trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // def printer(js: JSON): Unit =
  //   println(toString(js))

  def toString(js: JSON): String = js match {
    case JNull => "JNull"
    case JNumber(jNumber) => "JNumber("+jNumber.toString()+")"
    case JString(jString) => "JString("+jString+")"
    case JBool(jBool) => "JBool("+jBool.toString()+")"
    case JArray(jArray) => {
      val stringArray = jArray.map(json => "\t"+toString(json))
      "JArray("+stringArray.foldLeft("")(
        (acc: String, string: String) => acc + "\n" + string
      )+")"
    }
    case JObject(mp) => "JObject("+mp.foldRight("")(
      (kv: Tuple2[String, JSON], acc: String) =>
      acc + "\n" + kv._1 + "\t:\t" + toString(kv._2)
    )+")"
  }


  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def array = surround("[","]")(
      value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"
    def obj = surround("{","}")(
      keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"
    def keyval = escapedQuoted ** (":" *> value)
    def lit = scope("literal") {
      "null".as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }
}

/**
 * JSON parsing example.
 */
object JSONExample extends App {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  val P = fpinscala.answers.parsing.Reference
  import fpinscala.answers.parsing.ReferenceTypes.Parser

  def printResult[E](e: Either[E,JSON]) =
    e.fold(println, println)
  //e.fold((err: E) => println(err), (json: JSON) => println(JSON))

  def printResult2[E](e: Either[E,JSON]): Unit =
    e.fold(println, (js: JSON) => println(JSON.toString(js)))
  
  val json: Parser[JSON] = JSON.jsonParser(P)
  println("well formed JSON")
  println(jsonTxt)
  println("result")
  printResult2 { P.run(json)(jsonTxt) }
  println("-----------------------------")
  println("malformed JSON")
  println(malformedJson1)
  println("result")
  printResult2 { P.run(json)(malformedJson1) }
  println("-----------------------------")
  println("malformed JSON 2")
  println(malformedJson2)
  println("result")
  printResult2 { P.run(json)(malformedJson2) }
}
