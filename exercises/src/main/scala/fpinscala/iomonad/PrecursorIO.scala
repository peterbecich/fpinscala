package fpinscala.iomonad
import scala.language.higherKinds
import scala.language.postfixOps


object IO0 {
                            /*

  Our first attempt at data type for representing computations that
  may perform I/O. Has a simple 'interpreter' baked in--the `run`
  function, which just returns `Unit`.

                             */
  trait IO { self =>
    def run: Unit
    def ++(io: IO): IO = new IO {
      def run = { self.run; io.run }
    }
  }
  object IO {
    def empty: IO = new IO { def run = () }
  }

                            /*

  The API of this `IO` type isn't very useful.  Not many operations
  (it is only a monoid), and not many laws to help with reasoning. It
  is completely _opaque_. Also cannot represent _input_ effects, like
  reading from console, for instance:

                             */

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  // Ordinary code with side effects
  def converter: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  // A pure version is not possible!
  /*
  def converter: IO = {
    val prompt: IO = PrintLine("Enter a temperature in degrees fahrenheit: ")
    // now what ???
  }
  */
}

object IO1 {
                            /*

  We need a way for our `IO` actions to yield a result of some
  meaningful type. We do this by adding a type parameter to `IO`,
  which now forms a `Monad`.
                             */

  sealed trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] =
      new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] { def run = f(self.run).run }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }

    def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
    // concepts from next chapter -- Ref
    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO { value = a; a }
      def get: IO[A] = IO { value }
      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }
  }

  // We can now express the example

  def ReadLine: IO[String] = IO.apply[String](readLine)
  //                     IO { readLine }
  // syntactic sugar for IO.apply[String](readLine)
  def PrintLine(msg: String): IO[Unit] = IO.apply[Unit](println(msg))
    //IO { println(msg) }
  import IO0.fahrenheitToCelsius

  // def converter: IO[Unit] = for {
  //   _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
  //   d <- ReadLine.map(_.toDouble)
  //   _ <- PrintLine(fahrenheitToCelsius(d).toString)
  // } yield ()

  val tempPrompt: IO[Unit] = PrintLine("Enter a temperature in degrees Fahrenheit: ")
  val tempEntered: IO[Double] = ReadLine.map(_.toDouble)
  val printCelsius: Double => IO[Unit] = 
    (fahrenheit: Double) => PrintLine(
      fahrenheitToCelsius(fahrenheit).toString)

  def converter: IO[Unit] = tempPrompt.flatMap(Unit =>
    tempEntered.flatMap(printCelsius)
  )
  //                                  ^^
  //                          Unit => foo syntax works here
  //               but not in type of pattern match:
 //                  case Suspend(r: Unit => A) => ...


  /*                         Some other examples                      */

  import IO._ // import all the `IO` combinators that come from `Monad`

  // An `IO[Unit]` that reads a line from the console and echoes it back.
  val echo = ReadLine.flatMap(PrintLine)

  // Parses an `Int` by reading a line from the console.
  val readInt: IO[Int] = ReadLine.map(_.toInt)

  // Parses an `(Int,Int)` by reading two lines from the console.
  val readInts: IO[(Int,Int)] = readInt ** readInt

  // Repeat `converter` 5 times, discarding the results (which are
  // just `Unit`). We can replace `converter` here with any `IO`
  // action we wished to repeat 5 times (ex: `echo` or `readInts`).
  val prompts: IO[Unit] = replicateM_(5)(converter)

  // An `IO[List[String]]` that will read 10 lines from the console and
  // return the list of results.
  val lines: IO[List[String]] = replicateM(10)(ReadLine)

                            /*

  Larger example using various monadic combinators. Sample run:

     The Amazing Factorial REPL, v2.0
     q - quit
     <number> - compute the factorial of the given number
     <anything else> - bomb with horrible error
     3
     factorial: 6
     7
     factorial: 5040
     q

                             */
  val helpstring = """
  | The Amazing Factorial REPL, v2.0
  | q - quit
  | <number> - compute the factorial of the given number
  | <anything else> - bomb with horrible error
  """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result


  // same confusion that has shown up in other conversions of
  // for comps to explicit flatMaps and maps...
  // def factorial(n: Int): IO[Int] = {
  //   val oneToN: Stream[Int] = (1 to n).toStream
  //   val iOIORefOne: IO[IORef[Int]] = ref(1)
  //   iOIORefOne.flatMap((acc: Int) => {
  //     // def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
  //     val ioUnit: IO[Unit] = 
  //       foreachM (oneToN) ((i: Int) => acc.modify(_ * i).skip)
  //     acc
  //   }
  //   )
  // }



  val factorialREPL: IO[Unit] = sequence_(
    IO { println(helpstring) },
    doWhile { IO { readLine } } { line =>
      val ok = line != "q"
      when (ok) { for {
        n <- factorial(line.toInt)
        _ <- IO { println("factorial: " + n) }
      } yield () }
    }
  )
}

object IO1Tests {
  import IO1._
  val fibPrompt: IO[Unit] = PrintLine("Enter n and the nth Fibonacci number will print")
  val fib: IO[Int] = readInt.flatMap((n: Int) => factorial(n))
  val fibPrint: IO[Unit] = fib.flatMap((f: Int) => PrintLine(f.toString))
  val fibSequence: IO[Unit] = IO1.IO.sequence_(fibPrompt, fibPrint)

  def main(args: Array[String]): Unit = {
    converter.run
    println("single Fibonacci number")
    fibSequence.run
    println("Factorial REPL")
    factorialREPL.run

  }
}

object FactorialREPL extends App {

  import IO1._

  println("Factorial REPL")
  factorialREPL.run

}


object IO2a {

  /*
  The previous IO representation overflows the stack for some programs.
  The problem is that `run` calls itself recursively, which means that
  an infinite or long running IO computation will have a chain of regular
  calls to `run`, eventually overflowing the stack.

  The general solution is to make the `IO` type into a data type that we
  interpret using a tail recursive loop, using pattern matching.
  */

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: Function0[A]) extends IO[A]
  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  // now IO is object rather than class
  object IO extends Monad[IO] { // Notice that none of these operations DO anything
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
    def suspend[A](a: => IO[A]) =
      Suspend(() => ()).flatMap { _ => a }
  }

  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  val p: IO[Unit] = IO.forever(printLine("Still going..."))

  val actions: Stream[IO[Unit]] =
    Stream.fill(100000)(printLine("Still going..."))
  val composite: IO[Unit] =
    actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }

  // effectively turning (y flatMap g) flatMap f into y flatMap (a => g(a) flatMap f)

  // There is only one sensible way to implement this as a
  // tail-recursive function, the one tricky case is left-nested
  // flatMaps, as in `((a flatMap f) flatMap g)`, which we
  // reassociate to the right as `a flatMap (ar => f(a) flatMap g)`
  //                               ^^ tail recursive
  // @annotation.tailrec def run[A](io: IO[A]): A = {
  //   type rType = Unit => A
  //   type fType = A => IO[A]
  //   type gType = A => IO[A]
  //   io match {
  //     case Return(a: A) => a
  //     case Suspend(r: rType) => r()
  //     case FlatMap(x: IO[A], f: fType) => x match {
  //       case Return(a: A) => run(f(a))
  //       case Suspend(r: rType) => run(f(r()))
  //       case FlatMap(y: IO[A], g: gType) => 
  //         run(y flatMap (a => g(a) flatMap f))
  //     }
  //   }
  // }
  @annotation.tailrec def run[A](io: IO[A]): A = {
    io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => 
          run {
            y flatMap (a => g(a) flatMap f)
          }
      }
    }
  }
}

object IO2aTests {
  import IO2a._

  /*
   https://github.com/fpinscala/fpinscala/wiki/Errata
   Pg 240: REPL session has a typo, should be:

   val g = List.fill(100000)(f).foldLeft(f) {
   (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
   }

   Note: we could write a little helper function to make this nicer:

   def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }

   val g = List.fill(100000)(f).foldLeft(f) {
   (a, b) => x => suspend { a(x).flatMap(b) }
   }

   */
  val f: Int => IO[Int] = (x: Int) => Return(x)
  val g: Int => IO[Int] =
    List.fill(100000)(f).foldLeft(f){
      (x: Function1[Int,IO[Int]],
        y: Function1[Int,IO[Int]]) => {
        (i: Int) => IO.suspend(x(i).flatMap(y))
      }//: Function1[Int,IO[Int]]
    }//: Int => IO[Int]

  val h: Int => Int = (x: Int) => x
  val j: Int => Int = List.fill(10000)(h).foldLeft(h){
    (x: Function1[Int,Int], y: Function1[Int,Int]) =>
    x.compose(y)
  }




  def main(args: Array[String]): Unit = {
    println("using the IO2a monad, which makes 'run' tail recursive")
    //p.run
    //IO2a.run(IO2a.p)
    println(IO2a.run(g(40)))

    // println("naive way")
    // println("composing 10,001 copies of (x:Int)=>x without tail recursion")
    // j(40)
    // [error] (run-main-5) java.lang.StackOverflowError
    // java.lang.StackOverflowError


  }
}

object IO2b {

  /*
   * As it turns out, there's nothing about this data type that is specific
   * to I/O, it's just a general purpose data type for optimizing tail calls.
   * Here it is, renamed to `TailRec`. This type is also sometimes called
   * `Trampoline`, because of the way interpreting it bounces back and forth
   * between the main `run` loop and the functions contained in the `TailRec`.
   */

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    def flatMap[A,B](tra: TailRec[A])(f: A => TailRec[B]): TailRec[B] = tra flatMap f
    def suspend[A](a: => TailRec[A]): TailRec[A] =
      Suspend(() => ()).flatMap{ _ => a }
  }

  @annotation.tailrec def run[A](t: TailRec[A]): A = t match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      /*
       FlatMap(
         FlatMap(y: TailRec[A], g: A => TailRec[A]):
           TailRec[A], 
       f: A => TailRec[B]): TailRec[B] => 
       run(
         y.flatMap(
           (a: A) => g(a).flatMap(f): TailRec[B]
         ): TailRec[B]
       ): B

       Notice this is not only a translation of TailRec[A]
       to a tail recursive form...
       It is TailRec[A] => A, in tail recursive form
       */
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

object IO2bTests {
  import IO2b._
  import fpinscala.iomonad.Monad
  import fpinscala.laziness.Stream
  import fpinscala.laziness._

  // object streamMonad extends Monad[Stream] {
  //   def unit[A](a: => A): Stream[A] = Stream.apply(a)
  //   def flatMap[A,B](sa: Stream[A])(aSb: A => Stream[B]): Stream[B] =
  //     sa.flatMap(aSb)
  // }

  // val streamHundred = Stream.from(1).take(100)

  //val naiveSum = streamHundred.foldRight(0)(_+_)
  //val naiveSum = streamHundred.

  // for Streams of definite length only...
  // def sum(s: Stream[Int]):  = s match {
  //   case Cons(h, t) => Con

  // val tailRecStreamHundred: TailRec[Stream[Int]] = TailRec.unit(streamHundred)


  // Stream is too complicated for this
  // Just copy section 13.3.2: function g passed a value
  // through an identity function 10,000 times

  // stack overflow
  // val f = (x: Int) => x
  // val g = List.fill(10000)(f).foldLeft(f){
  //   (x: Function1[Int,Int], y: Function1[Int,Int]) => x.compose(y)
  // }

  val f: Int => TailRec[Int] = (i: Int) => Return(i)

  val g: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f){
      (x: Function1[Int, TailRec[Int]], y: Function1[Int, TailRec[Int]])
      => {
        (i: Int) => TailRec.suspend(x(i).flatMap(y))
      }
    }


  def main(args: Array[String]): Unit = {
    println("using the IO2b monad (TailRec)")
    println("Only difference with IO2a is name of trait/object")
    println("Revealed that trampolining is not limited to I/O")

    val gForty = g(40)

    print("g(40) = ")
    println(gForty)

    print("run(g(40)) = ")
    println(run(gForty))


    // do Stream example in Free monad
    //println("Imagine a Stream of integers which we want to recursively sum")


  }
}

import fpinscala.parallelism.Nonblocking._

object IO2c {


  /*
   * We've solved our first problem of ensuring stack safety, but we're still
   * being very inexplicit about what sort of effects can occur, and we also
   * haven't found a way of describing asynchronous computations. Our `Suspend`
   * thunks will just block the current thread when run by the interpreter.
   * We could fix that by changing the signature of `Suspend` to take a `Par`.
   * We'll call this new type `Async`.
   */

  sealed trait Async[A] { // will rename this type to `Async`
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))

  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A] // notice this is a `Par`
  case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

  // Async IO
  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A,B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
    def suspend[A](a: => Async[A]): Async[A] =
      Suspend(Par.unit(())).flatMap{_ => a }

  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec def step[A](async: Async[A]): Async[A] =
    async match {
      case FlatMap(FlatMap(x, f), g) =>
        step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => async
    }

  // Async[A] => Par[A]
  def run[A](async0: Async[A]): Par[A] = {
    val async1: Async[A] = step(async0)
    val runOut: Par[A] = async1 match {
      case Return(a) => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match {
        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
        case _ => sys.error("Impossible, since `step` eliminates these cases")
      }
    }
    runOut
  }
  // The fact that `run` only uses the `unit` and `flatMap` functions of
  // `Par` is a clue that choosing `Par` was too specific of a choice,
  // this interpreter could be generalized to work with any monad.
}
object IO2cTests {
  import IO2c._

  val f: Int => Async[Int] = (i: Int) => Async.suspend(Async.unit(i))

  val g: Int => Async[Int] =
    List.fill(10000)(f).foldLeft(f){
      (x: Function1[Int, Async[Int]], y: Function1[Int, Async[Int]]) => {
        (i: Int) => Async.suspend(x(i).flatMap(y))
      }
    }

  import java.util.concurrent.ExecutorService
  import java.util.concurrent.Executors

  def main(args: Array[String]): Unit = {
    val service = Executors.newFixedThreadPool(5)
    println(Thread.currentThread())


    println("using the IO2c monad (Async)")

    val gForty: Async[Int] = g(40)

    print("g(40) = ")
    println(gForty)

    print("run(g(40)) = ")
    println(run(gForty))

    val gFortyPar: Par[Int] = run(gForty)

    print(s"Par.run(gFortyPar)(service) = ")
    val i = Par.run(service)(gFortyPar)

    println(i)
    // do Stream example in Free monad
    //println("Imagine a Stream of integers which we want to recursively sum")
  }
}

