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
    println("Fibonacci REPL")
    factorialREPL.run

  }
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


object IO2c {

  import fpinscala.parallelism.Nonblocking._

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
    // def suspend[A](a: => Async[A]): Async[A] =
    //   Suspend(() => ()).flatMap{_ => a }

  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A] // notice this is a `Par`
  case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

  // Async IO
  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A,B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
    // def suspend[A](a: => Async[A]): Async[A] =
    //   Suspend(() => ()).flatMap{_ => a }

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

  val f: Int => Async[Int] = (i: Int) => Return(i)

  // val g: Int => Async[Int] =
  //   List.fill(10000)(f).foldLeft(f){
  //     (x: Function1[Int, Async[Int]],
  //       y: Function1[Int, Async[Int]]) => {
  //       (i: Int) => Async.suspend(x(i).flatMap(y))
  //     }
  //   }


  // def main(args: Array[String]): Unit = {
  //   println("using the IO2c monad (Async)")

  //   val gForty = g(40)

  //   print("g(40) = ")
  //   println(gForty)

  //   print("run(g(40)) = ")
  //   println(run(gForty))


  //   // do Stream example in Free monad
  //   //println("Imagine a Stream of integers which we want to recursively sum")


  // }
}



object IO3 {
  import fpinscala.parallelism.Nonblocking.Par

  /*
  We can generalize `TailRec` and `Async` to the type `Free`, which is
  a `Monad` for any choice of `F`.
  */

  sealed trait Free[F[_],A] {
    // answers for exercise 13.1
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F,B] =
      flatMap(f andThen (Return(_)))
    // generalize the helper function in Errata to Free from TailRec
    // def suspend: Free[F,A] =
    //   IO3.suspend(this)
  }

  // def suspend[F[_],A](a: => Free[F,A]): Free[F,A] =
  //   Suspend(() => ()).flatMap { _ => a }

  def suspend[F[_],A](a: => TailRec[A]): TailRec[A] =
    Suspend(() => ()).flatMap { _ => a }

  case class Return[F[_],A](a: A) extends Free[F, A]
  case class Suspend[F[_],A](s: F[A]) extends Free[F, A]
  // TailRec/Suspend(Function0[A])
  case class FlatMap[F[_],A,B](s: Free[F, A],
    f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  // Exercise 1: Implement the free monad
  // Using def instead of 'object freeMonad extends Monad...'
  // because of type parameter?
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = 
    new Monad[({type f[a] = Free[F,a]})#f] {
      // abstract primitives unit and flatMap made concrete
      // why does IO1.IO monad implement apply?
      def unit[A](a: => A): Free[F,A] = Return(a)
      def flatMap[A,B](freeA: Free[F,A])(
        aFreeB: A => Free[F,B]): Free[F,B] = 
        freeA.flatMap(aFreeB)
      // freeA match {
      //   case Return[F,A](a) => aFreeB(a)
      //   case Suspend[F,A](
      // }
    }
  // Exercise 2: Implement a specialized `Function0` interpreter.
  // narrow scope for TailRec
  // Exercise 2: Implement a specialized `Function0` interpreter.
  @annotation.tailrec
  def runTrampoline2[A](a: Free[Function0,A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x,f) => x match {
      case Return(a) => runTrampoline2 { f(a) }
      case Suspend(r) => runTrampoline2 { f(r()) }
      case FlatMap(a0,g) => runTrampoline2 {
        a0 flatMap { a0 => g(a0) flatMap f }
      }
    }
  }

  @annotation.tailrec
  def runTrampoline[A](tra: Free[Function0,A]): A =
    tra match {
      // Return(A)
      case Return(a1) => a1
      // Suspend(Function0[A])
      case Suspend(function0A1) => function0A1()
      // FlatMap(Free[Function0[_],A], A=>Free[Function0,B]]
      case FlatMap(free1, aFree2) => free1 match {
        // Return(A)
        case Return(a2) => runTrampoline(aFree2(a2))
        // Suspend(Function0[A])
        case Suspend(function0A) => runTrampoline(aFree2(function0A()))
        case FlatMap(a0,g) =>
          runTrampoline {
            a0 flatMap { a0 => g(a0) flatMap aFree2 }
          }
      }
    }
  @annotation.tailrec
  def runTrampoline3[A](tra: Free[Function0,A]): A =
    tra match {
      // Return(A)
      case Return(a1) => a1
      // Suspend(Function0[A])
      case Suspend(function0A1) => {
        val a1 = function0A1()
        a1
      }
      // FlatMap(Free[Function0[_],A], A=>Free[Function0,A]]
      case FlatMap(free1, aFree2) => free1 match {
        // Return(A)
        case Return(a2) => {
          val free2 = aFree2(a2)
          runTrampoline3(free2)
        }
        // Suspend(Function0[A])
        case Suspend(function0A) => {
          val a2 = function0A()
          val free2 = aFree2(a2)
          runTrampoline3(free2)
        }
        case FlatMap(a0,g) =>
          runTrampoline3 {
            a0 flatMap { a0 => g(a0) flatMap aFree2 }
          }
      }
    }

  // http://stackoverflow.com/a/21640639/1007926
  //import scala.reflect.runtime.universe._
  //import shapeless.Typeable._

  /*
   runTrampoline4 is identical to runTrampoline, and has expanded
   syntax.  It demonstrates that for some reason this breaks
   tail recursion.
   I think this has something to do with writing a Unit to a val.  
   This does not happen in runTrampoline.

   Possibly related to: http://stackoverflow.com/questions/31058950/scala-type-annotations-make-tail-recursion-check-fail
   */
  //@annotation.tailrec
  def runTrampoline4[A](tra: Free[Function0,A]): A =
    tra match {
      // Return(A)
      case Return(a1: A) => a1
      // Suspend(Function0[A])
      case Suspend(function0A1: Function0[A]) => function0A1()
      // FlatMap(Free[Function0[_],A], A=>Free[Function0,A]]
      case FlatMap(free1: Free[Function0,A],
        aFree2: Function1[A,Free[Function0,A]]) => free1 match {
        // Return(A)
        case Return(a2: A) => runTrampoline4(aFree2(a2))
        // Suspend(Function0[A])
        case Suspend(function0A: Function0[A]) =>
          runTrampoline4(aFree2(function0A()))
        case FlatMap(a0: A, g: Function1[A,Free[Function0,A]]) =>
          runTrampoline4 {
            a0 flatMap { a0 => g(a0) flatMap aFree2 }
          }
      }
    }


  // Exercise 3:
  // Implement a `Free` interpreter which works for any `Monad`

  // run's signature for TailRec[A]:
  // run[Function0[_],A](Free[Function0,A])(Monad[Function0]):
  //                                        Function0[A]
  // will return Par[A], Async[A]...
  
  def run[F[_],A](freeFA: Free[F,A])(implicit F: Monad[F]): F[A] =
    step(freeFA) match {
      case Return(a) => F.unit(a)
      case Suspend(fa) => fa //F.flatMap(fa)((a: A) => run(a))
      case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
        // ^^ why is this different to typechecker
        // than what is below??
      // case FlatMap(freeFA2, f) => freeFA2 match {
      //   case Suspend(fa2) => F.flatMap(fa2)((a: A) => run(f(a)))
      //   case _ => sys.error("run(FlatMap(...)) in impossible state.  See listing 13.5")
      // }
    }
  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec
  def step[F[_],A](freeFA: Free[F,A]): Free[F,A] =
    freeFA match {
      // FlatMap(FlatMap(Free[F,A], A=>Free[F,A]), A=>Free[F,A])
      case FlatMap(FlatMap(x,f),g) =>
        step(x.flatMap(a => f(a).flatMap(g)))
        /*
         Why does type annotation of 'a' matter?
         type mismatch;  
         found   : A => fpinscala.iomonad.IO3.Free[F,A]  
         required: Any => fpinscala.iomonad.IO3.Free[F,A]  
         Note: implicit value parMonad is not applicable here
         because it comes after the application point and it
         lacks an explicit result type

         because 'a' without type annotation can be Any?
         */
        //step(x.flatMap((a: A) => f(a).flatMap(g)))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => freeFA
    }

  /*
  The type constructor `F` lets us control the set of external requests our
  program is allowed to make. For instance, here is a type that allows for
  only console I/O effects.
  */

  //import fpinscala.parallelism.Nonblocking.Par

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A

    // other interpreters
    def toState: ConsoleState[A]
    def toReader: ConsoleReader[A]
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk: Function0[Option[String]] = () => run

    def run: Option[String] =
      try Some(readLine())
      catch { case e: Exception => None }

    def toState = ConsoleState { bufs =>
      bufs.in match {
        case List() => (None, bufs)
        case h :: t => (Some(h), bufs.copy(in = t))
      }
    }
    def toReader = ConsoleReader { in => Some(in) }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
    def toReader = ConsoleReader { s => () } // noop
    def toState = ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) } // append to the output
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  /*
  How do we actually _run_ a `ConsoleIO` program? We don't have a `Monad[Console]`
  for calling `run`, and we can't use `runTrampoline` either since we have `Console`,
  not `Function0`. We need a way to translate from `Console` to `Function0`
  (if we want to evaluate it sequentially) or a `Par`.

  We introduce the following type to do this translation:
  */

  /* Translate between any `F[A]` to `G[A]`. */
  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }

  type ~>[F[_], G[_]] = Translate[F,G] // gives us infix syntax `F ~> G` for `Translate[F,G]`

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]) = Par.fork { Par.flatMap(a)(f) }
  }

  //@annotation.tailrec
  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(
                           implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) =>
        G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ =>
        sys.error("Impossible, since `step` eliminates these cases")
    }

  val consoleToFunction0: Translate[Console, Function0] =
    new (Console ~> Function0) {
    //  ^^ instance of trait Translate[Console[_], Function0[_]]
      def apply[A](a: Console[A]) = a.toThunk
    }
  val consoleToPar: Translate[Console, Par] =
    new (Console ~> Par) {
      def apply[A](a: Console[A]) = a.toPar
    }

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    runFree[Console,Function0,A](a)(consoleToFunction0)
  // uses implicit Monad[Function0]
  def runConsolePar[A](a: Free[Console,A]): Par[A] =
    runFree[Console,Par,A](a)(consoleToPar)
  // uses implicit Monad[Par]

  /*
  The `runConsoleFunction0` implementation is unfortunately not stack safe,
  because it relies of the stack safety of the underlying monad, and the
  `Function0` monad we gave is not stack safe. To see the problem, try
  running: `freeMonad.forever(Console.printLn("Hello"))`.
  */

  // Exercise 4 (optional, hard): Implement `runConsole` using `runFree`,
  // without going through `Par`. Hint: define `translate` using `runFree`.

  def translate[F[_],G[_],A](freefa: Free[F,A])(fg: F ~> G): Free[G,A] = {
    // from answers...
    type FreeG[A] = Free[G,A]
    val translation: Translate[F,FreeG] =
      new Translate[F,FreeG]{
        def apply[A](fa: F[A]): FreeG[A] = Suspend(fg(fa))
      }
    runFree(freefa)(translation)(freeMonad[G])
  }


  def runConsole[A](freeConsoleA: Free[Console,A]): A = {
    // val function0A: () => A = runConsoleFunction0(a)
    // function0A()
    // ^^^ not stack safe?
    // val freeFunction0A: Free[Function0,A] =
    //   translate(freeConsoleA)(consoleToFunction0)
    // val function0A: Function0[A] =
    //   runFree(

    val translation: Translate[Console,Function0] =
      new Translate[Console,Function0] {
        def apply[A](ca: Console[A]): Function0[A] = ca.toThunk
      }
    //val translated: Free[Function0,A] =
    val function0A: Function0[A] =
      runFree(freeConsoleA)(translation)(function0Monad)
    val a: A = function0A()
    a
  }

  /*
  There is nothing about `Free[Console,A]` that requires we interpret
  `Console` using side effects. Here are two pure ways of interpreting
  a `Free[Console,A]`.
  */
  import Console._

  case class Buffers(in: List[String], out: Vector[String])

  // A specialized state monad
  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    def map[B](f: A => B): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        (f(a), s1)
      }
    def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        f(a).run(s1)
      }
  }
  object ConsoleState {
    implicit val monad = new Monad[ConsoleState] {
      def unit[A](a: => A) = ConsoleState(bufs => (a,bufs))
      def flatMap[A,B](ra: ConsoleState[A])(f: A => ConsoleState[B]) = ra flatMap f
    }
  }

  // A specialized reader monad
  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }
  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => a)
      def flatMap[A,B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
    }
  }

  val consoleToState =
    new (Console ~> ConsoleState) { def apply[A](a: Console[A]) = a.toState }
  val consoleToReader =
    new (Console ~> ConsoleReader) { def apply[A](a: Console[A]) = a.toReader }

  /* Can interpet these as before to convert our `ConsoleIO` to a pure value that does no I/O! */
  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console,ConsoleReader,A](io)(consoleToReader)

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] =
    runFree[Console,ConsoleState,A](io)(consoleToState)

  // So `Free[F,A]` is not really an I/O type. The interpreter `runFree` gets
  // to choose how to interpret these `F` requests, and whether to do "real" I/O
  // or simply convert to some pure value!

  // NB: These interpretations are not stack safe for the same reason,
  // can instead work with `case class ConsoleReader[A](run: String => Trampoline[A])`,
  // which gives us a stack safe monad

  // We conclude that a good representation of an `IO` monad is this:
  type IO[A] = Free[Par, A]

  /*
   * Exercise 5: Implement a non-blocking read from an asynchronous file channel.
   * We'll just give the basic idea - here, we construct a `Future`
   * by reading from an `AsynchronousFileChannel`, a `java.nio` class
   * which supports asynchronous reads.
   */

  import java.nio._
  import java.nio.channels._

  def read(file: AsynchronousFileChannel,
    fromPosition: Long,
    numBytes: Int): Par[Either[Throwable, Array[Byte]]] = {
    val io: IO[Either[Throwable, Array[Byte]]] = Async {
      // AsychronousFileChannel =>
      // Free[Par, Either[Throwable, Array[Byte]]] =>
      // Par[Either[Throwable, Array[Byte]]]
      (cb: Either[Throwable,Array[Byte]] => Unit) => {
        // http://stackoverflow.com/questions/4841340/what-is-the-use-of-bytebuffer-in-java
        val buf = ByteBuffer.allocate(numBytes)
        // val future: java.util.concurrent.Future[Integer] =
        //   file.read(buf, fromPosition)
        // future.
        // from answers
        file.read(
          buf, fromPosition,
          (), new CompletionHandler[Integer, Unit] {
            def completed(bytesRead: Integer, ignore: Unit) = {
              val arr = new Array[Byte](bytesRead)
              buf.slice.get(arr, 0, bytesRead)
              cb(Right(arr))
            }
            def failed(err: Throwable, ignore: Unit) =
              cb(Left(err))
          }
        )
      }
    }
    val par: Par[Either[Throwable, Array[Byte]]] =
      run(io)(parMonad)
    par
  }




  // Provides the syntax `Async { k => ... }` for asyncronous IO blocks.
  def Async[A](cb: (A => Unit) => Unit): IO[A] =
    Suspend(Par.async(cb))

  // Provides the `IO { ... }` syntax for synchronous IO blocks.
  def IO[A](a: => A): IO[A] = Suspend { Par.delay(a) }

  trait Source {
    def readBytes(
      numBytes: Int,
      callback: Either[Throwable, Array[Byte]] => Unit): Unit
  }

  val sourceExample: Source = new Source {
    def readBytes(
      numBytes: Int,
      callback: Either[Throwable, Array[Byte]] => Unit): Unit = {
      val incrementingBytes: Array[Byte] =
        (1 to numBytes).map(_.toByte).toArray
      val rightBytes = Right(incrementingBytes)
      callback(rightBytes)
    }
  }

  def nonblockingRead(source: Source, numBytes: Int):
      IO[Either[Throwable,Array[Byte]]] =
    Async {
      (cb: Either[Throwable,Array[Byte]] => Unit) =>
      source.readBytes(numBytes, cb)
    }

  trait Files[A]
  case class OpenFileRead(file: String) extends Files[HandleR]
  case class OpenFileWrite(file: String) extends Files[HandleW]
  case class ReadFileLine(h: HandleR) extends Files[Option[String]]
  case class WriteFileLine(h: HandleW, line: String) extends Files[Unit]

  trait HandleR
  trait HandleW

}

object IO3Tests {
  import IO3._
  import fpinscala.iomonad.Monad
  import fpinscala.laziness.Stream

  object streamMonad extends Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.apply(a)
    def flatMap[A,B](sa: Stream[A])(aSb: A => Stream[B]): Stream[B] =
      sa.flatMap(aSb)
  }

  // val streamHundred = Stream.from(1).take(100)

  // val naiveSum = streamHundred.foldRight(0)(_+_)

  // val tailRecStreamHundred: [Stream[Int]] = TailRec.unit(streamHundred)


  val recursiveFactorial: Int => Int = (i: Int) =>
  if(i>1) i * recursiveFactorial(i-1)
  else 1

  // only saves one recursion...
  // val tailRecursiveFactorial: Int => TailRec[Int] =
  //   (i: Int) => Return(recursiveFactorial(i))

  // type TailRec[A] = Free[Function0, A]
  // def runTrampoline[A](tra: Free[Function0,A]): A
  // def run[F[_],A](freeFA: Free[F,A])(implicit F: Monad[F]): F[A]
  // def run[F[_],A](
  //   freeFA: Free[Function0,A])(implicit F: Monad[Function0]):
  //   Function0[A]
  // def step[F[_],A](freeFA: Free[F,A]): Free[F,A]
  // def step[F[_],A](freeFA: Free[Function0,A]): Free[Function0,A]

  /*
  The opposite of tail recursion is general recursion
  tailRecursiveFactorial optimizes an intentionally
  generally recursive factorial function.
  It would be easy enough to write a tail recursive factorial
  without needing CPS.  That's not the point of this exercise.
    */
  val tailRecursiveFactorial: Long => TailRec[Long] =
    (i: Long) =>
  suspend {
    if(i>1) {
      //println(i)
      tailRecursiveFactorial(i-1).flatMap{
        (x: Long) => {
          //println(x*i)
          Suspend(()=>x*i)
        }
      }
      // val next: TailRec[Int] = tailRecursiveFactorial(i-1)
      // val flatten: Int => TailRec[Int] =
      //   (i1: Int) => Return(i*i1)
      // val flattened: TailRec[Int] = FlatMap(next, flatten)
      // flattened
    } else Return(1)
  }


  val tailRecursiveFactorial2: Int => TailRec[BigInt] =
    (i: Int) =>
  suspend {
    if(i>1) {
      //println(i)
      tailRecursiveFactorial2(i-1).flatMap{
        (x: BigInt) => {
          Suspend(()=>x*i)
        }
      }
    } else Return(1.toLong)
  }

  val tailRecursiveFactorial3: Int => TailRec[BigInt] =
    (i: Int) =>
    if(i>1) {
      //println(i)
      tailRecursiveFactorial2(i-1).flatMap{
        (x: BigInt) => {
          Return(x*i)
        }
      }
    } else Return(1.toLong)


  // http://matt.might.net/articles/by-example-continuation-passing-style/

  // page 240
  val id: Int => TailRec[Int] = (x: Int) => Return(x)
  val passThru: Int => TailRec[Int] =
    List.fill(100000)(id).foldLeft(id){
      (a: Function1[Int,TailRec[Int]],
        b: Function1[Int,TailRec[Int]]) => {
        (x: Int) => IO3.suspend{
          a(x).flatMap(b)
        }

      }
    }

  val passThru2: Int => TailRec[Int] =
    List.fill(999)(id).foldLeft(id){
      (a: Function1[Int,TailRec[Int]],
        b: Function1[Int,TailRec[Int]]) => {
        (x: Int) => a(x).flatMap(b)
      }
    }



  // def naiveFactorial2(fact: Int): Int = {
  //   val st: Stream[Int] = Stream.seq(fact,(i: Int)=>i-1,1)
  //   //st.foldRight(()=>1)(naiveFactorialLambda)

  // }
  // //def naiveFactorialLambda(i: Int, i2:=> Int): Int = i*i2

  // def tailRecursiveFactorial2(fact: Int): TailRec[Int] = {
  //   val streamInt: Stream[Int] = Stream.from(1).take(fact)

  // }

  // http://blog.higher-order.com/blog/2015/06/18/easy-performance-wins-with-scalaz/

  def ackermannNaive(m: Int, n: Int): Int = (m,n) match {
    case (0, _) => n+1
    case (m, 0) => ackermannNaive(m-1, 1)
    case (m, n) => ackermannNaive(m-1, ackermannNaive(m, n-1))
  }

  def tailRecAckermann: (Int,Int) => TailRec[Int] =
  (m: Int, n: Int) =>
  IO3.suspend {
    (m,n) match {
      case (0, _) => Return(n+1)
      case (m, 0) => tailRecAckermann(m-1,1)
      case (m, n) =>
        FlatMap(tailRecAckermann(m, n-1),
          (p:Int) => tailRecAckermann(m-1,p)
        )
    }
  }
  val tailRecAckermann2020: TailRec[Int] = tailRecAckermann(20,20)

  def main(args: Array[String]): Unit = {
    println("passThru: Int => TailRec[Int]")
    println("equivalent to")
    println("passThru: Int => Free[Function0, Int]")

    println("passThru123: TailRec[Int]")
    val passThru123: TailRec[Int] = passThru(123)
    println(runTrampoline(passThru123))
    println("Flawed pass through; fewer than 10000 calls to identity function for sake of demonstration.")
    println("Suspensions replaced with Returns, or nothing")
    val passThru1232: TailRec[Int] = passThru2(123)
    //println(passThru1232)
    // don't even bother trampolining it...
    // [error] (run-main-1d) java.lang.StackOverflowError

    println("-------------------------")



    println("Note that factorial isn't a great test for trampolining, because even the naive implemenation doesn't push much to the stack.  Fact(100) is only 99 recursions.")

    println("tailRecursiveFactorial: Long => TailRec[Long]")
    println("tailRecursiveFactorial(100)")
    val tailRecFact100: TailRec[Long] = tailRecursiveFactorial(100)
    println(tailRecFact100)
    println("runTrampoline")
    val fact100: Long = runTrampoline(tailRecFact100)
    println("factorial of 100")
    println(fact100)

    println("tailRecursiveFactorial2: Int => TailRec[BigInt]")
    println("tailRecursiveFactorial2(100)")
    val tailRecFact1002: TailRec[BigInt] = tailRecursiveFactorial2(100)
    println(tailRecFact1002)
    println("runTrampoline")
    val fact1002: BigInt = runTrampoline(tailRecFact1002)
    println("factorial of 100")
    println(fact1002)
    println("-------------------------")

    println("a flawed factorial")
    println("tailRecursiveFactorial3: Int => TailRec[BigInt]")
    println("tailRecursiveFactorial3(10)")
    val tailRecFact3: TailRec[BigInt] = tailRecursiveFactorial3(10)
    println(tailRecFact3)
    println("Notice the extra first-class function in un-interpreted factorial.  Two suspensions have been removed from this factorial function.")
    // println("runTrampoline")
    // val fact3: BigInt = runTrampoline(tailRecFact3)
    // println("factorial of 10")
    // println(fact3)
    println("Compare to tailRecursiveFactorial2")
    println("tailRecursiveFactorial2: Int => TailRec[BigInt]")
    println("tailRecursiveFactorial2(10)")
    val tailRecFact4: TailRec[BigInt] = tailRecursiveFactorial2(10)
    println(tailRecFact4)

    println("--------------------------")
    println("runTrampoline with expanded syntax")
    println("runTrampoline3(tailRecursiveFactorial(100))")
    println(runTrampoline3(tailRecFact1002))

    // println("naive Ackermann function")
    // println("ackermannNaive(20,20)")
    // println("[error] (run-main-0) java.lang.StackOverflowError")
    // //println(ackermannNaive(20,20))
    // // println("tail recursive Ackermann function")
    // println("tailRecAckermann(20,20)")
    // println(tailRecAckermann2020)
    // println("run trampoline")
    // println(runTrampoline(tailRecAckermann2020))
  }
}


object ConsoleTests {
  import IO3._
  import IO3.Console._
  import fpinscala.iomonad.Monad
  import fpinscala.laziness.Stream

  val helloConsole: ConsoleIO[Unit] =
    printLn("Hello console! (I can only interact with the console...).  Enter something:")

  // page 244
  val f1: Free[Console, Option[String]] =
    helloConsole.flatMap(_ => readLn)

  val f1NotStackSafe: Function0[Option[String]] =
    runConsoleFunction0(f1)
  val f1Result1: Option[String] = f1NotStackSafe()

  //val f1Runnable: Free[Function0, Option[String]] =

  // Free[Console, Unit]
  val yourName: ConsoleIO[Unit] =
    printLn("What's your name").flatMap(_ =>
      readLn.flatMap(opname =>
        opname match {
          case Some(name) => printLn(s"Hello, $name!")
          case None => printLn(s"Fine, be that way...")
        }
      )
    )
  val yourNameFunction0: Free[Function0, Unit] =
    translate(yourName)(consoleToFunction0)
    
  def main(args: Array[String]): Unit = {
    println("f1 obtained without stack safety")
    println(f1Result1)
    println("-------------------------")
    println("yourName: ConsoleIO[Unit]")
    println(yourName)
    println("runConsoleFunction0")
    runConsoleFunction0(yourName): Unit
    // println("runTrampoline")
    // runTrampoline(yourName)
    println("yourNameFunction0: Free[Function0, Unit]")
    runTrampoline(yourNameFunction0)
  }
}

object ConsoleReaderTests {
  import IO3._
  import IO3.Console._
  import fpinscala.iomonad.Monad
  import fpinscala.laziness.Stream

  val helloConsole: ConsoleIO[Unit] =
    printLn("Hello console! (I can only interact with the console...).  Enter something:")
  val somethingEntered: Free[Console, Option[String]] =
    helloConsole.flatMap(_ => readLn)

  val helloReader: ConsoleReader[Option[String]] =
    runConsoleReader(somethingEntered)


  // Free[Console, Unit]
  val yourName: ConsoleIO[Unit] =
    printLn("What's your name").flatMap(_ =>
      readLn.flatMap(opname =>
        opname match {
          case Some(name) => printLn(s"Hello, $name!")
          case None => printLn(s"Fine, be that way...")
        }
      )
    )

  val yourNameReader: ConsoleReader[Unit] = runConsoleReader(yourName)

  // val yourNameFunction0: Free[Function0, Unit] =
  //   translate(yourName)(consoleToFunction0)
    
  def main(args: Array[String]): Unit = {
    println("console reader")
    val somethingAlwaysEntered: Option[String] =
      helloReader.run("same thing, every time")
    println(s"entered: $somethingAlwaysEntered")

    yourNameReader.run("Fritz")
    // Unit evaporates...
  }
}


object AsynchronousReaderTests {
  import IO3._
  import IO3.Console._
  import fpinscala.iomonad.Monad
  import fpinscala.laziness.Stream
  import fpinscala.parallelism.Nonblocking.Par
    
  def main(args: Array[String]): Unit = {
    println("example Source; 64 incrementing bytes")
    sourceExample.readBytes(64,
      (either: Either[Throwable, Array[Byte]]) =>
      either match {
        case Left(throwable) => throw throwable
        case Right(arrByte) => println(arrByte)
      }
    )

    println("nonblocking read")
    // Free[Par, Either[Throwable,Array[Byte]]]
    val io: IO[Either[Throwable, Array[Byte]]] =
      nonblockingRead(sourceExample, 64)
    println("nonblocking read already initiated")
    println("contained within an IO")
    println(io)
    println("run(IO[...])(Monad[Par])")
    // val out: Either[Throwable, Array[Byte]] =
    //   run(io)(parMonad)
    val par: Par[Either[Throwable, Array[Byte]]] =
      run(io)(parMonad)
    println("Par[Either[Throwable, Array[Byte]]]")
    println(par)

    import java.util.concurrent.ExecutorService
    import java.util.concurrent.Executors

    val service = Executors.newFixedThreadPool(4)
    val eitherArrByte: Either[Throwable, Array[Byte]] =
      Par.run(service)(par)
    println("byte array or thrown error")
    eitherArrByte match {
      case Left(throwable) => throw throwable
      case Right(arrByte) => println(arrByte)
    }
    service.shutdown()

  }
}

object MonolithicLoopRead {
  import IO3._
  import fpinscala.iomonad.Monad
  import fpinscala.laziness.Stream
  import fpinscala.parallelism.Nonblocking.Par

  // page 252.  what is b?
  // def loop(f: HandleR, c: HandleW): Free[Files, Unit] =
  //   Suspend(ReadFileLine(f)).flatMap(line =>
  //     line match {
  //       case None => IO.unit(())
  //       case Some(s) => Suspend {
  //         WriteFileLine(fahrenheitToCelsius(s.toDouble))
  //       }.flatMap(_ => loop(f,c))
  //     }
    
  def main(args: Array[String]): Unit = {

  }
}


