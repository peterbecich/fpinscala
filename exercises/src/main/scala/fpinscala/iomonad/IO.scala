package fpinscala.iomonad
import scala.language.higherKinds
import scala.language.postfixOps

object IO3 {
  import fpinscala.parallelism.Nonblocking.Par

  sealed trait Free[F[_],A] {
    // answers for exercise 13.1
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F,B] =
      flatMap(f.andThen(Return(_)))
    // generalize the helper function in Errata to Free from TailRec
    // def suspend: Free[F,A] =
    //   IO3.suspend(this)
  }

  // def suspend[F[_],A](a: => Free[F,A]): Free[F,A] =
  //   Suspend(() => ()).flatMap { _ => a }

  // later, generalize these helper functions past TailRec
  // def return[A](a: A): TailRec[A] = Return(() => a)
  def suspend[A](a: => TailRec[A]): TailRec[A] =
    Suspend(() => ()).flatMap { _ => a }
  // def flatMap[A,B](f: A => TailRec[B])

  case class Return[F[_],A](a: A) extends Free[F, A]
  case class Suspend[F[_],A](s: F[A]) extends Free[F, A]
  // TailRec/Suspend(Function0[A])
  case class FlatMap[F[_],A,B](s: Free[F, A],
    f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]

  // Exercise 1: Implement the free monad
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

  // TailRec[A] == Free[Function0,A]
  def runTrampoline2[A](tailRecA: TailRec[A]): A = {
    val function0A: Function0[A] =
      run(tailRecA)(function0Monad)
    function0A()
  }

  @annotation.tailrec
  def runTrampoline[A](tra: TailRec[A]): A =
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
          runTrampoline(free2)
        }
        // Suspend(Function0[A])
        case Suspend(function0A: Function0[A]) => {
          val a2 = function0A()
          val free2 = aFree2(a2)
          runTrampoline(free2)
        }
        case FlatMap(a0,g) =>
          runTrampoline {
            a0 flatMap { a0 => g(a0) flatMap aFree2 }
          }
      }
    }

  // http://stackoverflow.com/a/21640639/1007926
  //import scala.reflect.runtime.universe._
  //import shapeless.Typeable._

  // run's signature for TailRec[A]:
  // run[Function0[_],A](Free[Function0,A])(Monad[Function0]): Function0[A]

  // for IO:
  // run[Par[_],A](Free[Par,A])(Monad[Par]): Par[A]
  //@annotation.tailrec
  def run[F[_],A](freeFA: Free[F,A])(
    implicit F: Monad[F]): F[A] = {
    //println(freeFA)
    step(freeFA) match {
      case Return(a) => F.unit(a): F[A]
      case Suspend(fa) => fa: F[A]
      case FlatMap(Suspend(r), f) => {
        //println("flatmap")
        F.flatMap(r)(a => run(f(a))(F)): F[A]
      }
    }
  }

  @annotation.tailrec
  def step[F[_], A](freeFA: Free[F,A]): Free[F,A] = {
    freeFA match {
      /*
       FlatMap(
       FlatMap(Free[F,A], A=>Free[F,A]),
       A=>Free[F,A]
       )
       */

      // book's version
      case FlatMap(
        FlatMap(x, f),
        g
      ) => step(x.flatMap((a: Any) => f(a).flatMap(g)))

      case FlatMap(Return(x), f) => step(f(x))
      case Suspend(_) => freeFA
      case Return(_) => freeFA
      case _ => freeFA
    }
  }


  /*
   The type constructor `F` lets us control the set of external requests our
   program is allowed to make. For instance, here is a type that allows for
   only console I/O effects.
   */

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
    def flatMap[A,B](a: Function0[A])(
      f: A => Function0[B]) =
      () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    //def flatMap[A,B](a: Par[A])(f: A => Par[B]) = Par.flatMap(a)(f)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]) =
      Par.fork { Par.flatMap(a)(f) }
  }

  // @annotation.tailrec
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


object PassThruFlawed {
  import IO3._
  // page 240
  val id: Int => TailRec[Int] = (x: Int) => Return(x)

  // not truly tail recursive
  def passThruFlawed(passes: Int):
      Function1[Int,TailRec[Int]] =
    List.fill(passes)(id).foldLeft(id){
      (a: Function1[Int,TailRec[Int]],
        b: Function1[Int,TailRec[Int]]) => {
        (x: Int) => a(x).flatMap(b)
      }
    }

  // 100 passes
  val passThru100 = passThruFlawed(100)
  // 10,000 passes
  val passThru10000 = passThruFlawed(10000)

  def main(args: Array[String]): Unit = {
    println("passThru: Int => TailRec[Int]")
    println("equivalent to")
    println("passThru: Int => Free[Function0, Int]")

    println("Flawed pass through; fewer than 10000 calls to identity function for sake of demonstration.")
    println("Suspensions replaced with Returns, or nothing")

    println("put Int 123 through 100 passes")
    println("tailRec123: TailRec[Int] = passThru100(123)")
    val tailRec123: TailRec[Int] = passThru100(123)
    println(runTrampoline(tailRec123))

    println("put Int 123 through 10,000 pases")
    println("tailRec1232: TailRec[Int] = passThru10000(123)")
    val tailRec1232: TailRec[Int] = passThru10000(123)
    println(runTrampoline(tailRec1232))
  }
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
  def passThru(passes: Int):
      Function1[Int,TailRec[Int]] =
    List.fill(passes)(id).foldLeft(id){
      (a: Function1[Int,TailRec[Int]],
        b: Function1[Int,TailRec[Int]]) => {
        (x: Int) => IO3.suspend{
          a(x).flatMap(b)
        }

      }
    }

  // 100 passes
  val passThru100 = passThru(100)
  // 10,000 passes
  val passThru10000 = passThru(10000)

  // http://stackoverflow.com/questions/12678099/ackermann-function-understanding

  def tailRecAckermann: (Int,Int) => TailRec[Int] =
    (m: Int, n: Int) =>
  IO3.suspend {
    //println(s"ackermann $m $n")
    (m,n) match {
      case (0, _) => Return(n+1)
      case (m, 0) if m>0 => tailRecAckermann(m-1,1)
      case (m, n) if m>0 && n>0 =>
        FlatMap(tailRecAckermann(m, n-1),
          (p:Int) => tailRecAckermann(m-1,p)
        )
    }
  }
  val tailRecAckermann11: TailRec[Int] =
    tailRecAckermann(1,1)
  val tailRecAckermann34: TailRec[Int] =
    tailRecAckermann(3,4)
  val tailRecAckermann2020: TailRec[Int] = tailRecAckermann(20,20)
  val tailRecAckermann42: TailRec[Int] =
    tailRecAckermann(4,2)

  def main(args: Array[String]): Unit = {
    println("passThru: Int => TailRec[Int]")
    println("equivalent to")
    println("passThru: Int => Free[Function0, Int]")

    println("put Int 123 through 100 passes")
    println("tailRec123: TailRec[Int] = passThru100(123)")
    val tailRec123: TailRec[Int] = passThru100(123)
    println(runTrampoline(tailRec123))

    println("put Int 123 through 10,000 pases")
    println("tailRec1232: TailRec[Int] = passThru10000(123)")
    val tailRec1232: TailRec[Int] = passThru10000(123)
    println(runTrampoline(tailRec1232))


    println("-------------------------")



    // println("Note that factorial isn't a great test for trampolining, because even the naive implemenation doesn't push much to the stack.  Fact(100) is only 99 recursions.")

    // println("tailRecursiveFactorial: Long => TailRec[Long]")
    // println("tailRecursiveFactorial(100)")
    // val tailRecFact100: TailRec[Long] = tailRecursiveFactorial(100)
    // println(tailRecFact100)
    // println("runTrampoline")
    // val fact100: Long = runTrampoline(tailRecFact100)
    // println("factorial of 100")
    // println(fact100)

    // println("tailRecursiveFactorial2: Int => TailRec[BigInt]")
    // println("tailRecursiveFactorial2(100)")
    // val tailRecFact1002: TailRec[BigInt] = tailRecursiveFactorial2(100)
    // println(tailRecFact1002)
    // println("runTrampoline")
    // val fact1002: BigInt = runTrampoline(tailRecFact1002)
    // println("factorial of 100")
    // println(fact1002)
    // println("-------------------------")

    // println("a flawed factorial")
    // println("tailRecursiveFactorial3: Int => TailRec[BigInt]")
    // println("tailRecursiveFactorial3(10)")
    // val tailRecFact3: TailRec[BigInt] = tailRecursiveFactorial3(10)
    // println(tailRecFact3)
    // println("Notice the extra first-class function in un-interpreted factorial.  Two suspensions have been removed from this factorial function.")
    // // println("runTrampoline")
    // // val fact3: BigInt = runTrampoline(tailRecFact3)
    // // println("factorial of 10")
    // // println(fact3)
    // println("Compare to tailRecursiveFactorial2")
    // println("tailRecursiveFactorial2: Int => TailRec[BigInt]")
    // println("tailRecursiveFactorial2(10)")
    // val tailRecFact4: TailRec[BigInt] = tailRecursiveFactorial2(10)
    // println(tailRecFact4)

    println("naive Ackermann function")
    println("ackermannNaive(20,20)")
    println("[error] (run-main-0) java.lang.StackOverflowError")
    //println(ackermannNaive(20,20))
    println("tail recursive Ackermann function")
    println("tailRecAckermann(1,1)")
    println(tailRecAckermann11)
    println("run trampoline")
    println(runTrampoline(tailRecAckermann11))
    println("tailRecAckermann(3,4)")
    println(tailRecAckermann34)
    println("run trampoline")
    println(runTrampoline(tailRecAckermann34))
    // println("tailRecAckermann(4,2)")
    // println(tailRecAckermann42)
    // println("run trampoline")
    // println(runTrampoline(tailRecAckermann42))


  }
}
object AckermannNaive {

  // http://blog.higher-order.com/blog/2015/06/18/easy-performance-wins-with-scalaz/

  def ackermannNaive(m: Int, n: Int): Int = (m,n) match {
    case (0, _) => n+1
    case (m, 0) => ackermannNaive(m-1, 1)
    case (m, n) => ackermannNaive(m-1, ackermannNaive(m, n-1))
  }
  def main(args: Array[String]): Unit = {
    println("naive Ackermann(3,4)")
    println(ackermannNaive(3,4))
    println("naive Ackermann(4,2)")
    println(ackermannNaive(4,2))
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
      helloReader.run("same thing... every time")
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
        case Right(arrByte) =>
          arrByte.toList.foreach((b: Byte) => printf("0x%02X\n", b))
      }
    )

    println("io: IO[Either[Throwable,Array[Byte]]] = nonblockingRead(sourceExample,64)")
    // Free[Par, Either[Throwable,Array[Byte]]]
    val io: IO[Either[Throwable, Array[Byte]]] =
      nonblockingRead(sourceExample, 64)
    println("nonblocking read initiated")
    println("io: IO[Either[Throwable,Array[Byte]]] = "+io)

    val par: Par[Either[Throwable, Array[Byte]]] =
      run(io)(parMonad)
    println("par: Par[Either[Throwable,Array[Byte]]] = run(IO[...])(Monad[Par]) = "+par)

    import java.util.concurrent.ExecutorService
    import java.util.concurrent.Executors

    val service = Executors.newFixedThreadPool(4)
    println("service = "+service)
    val eitherArrByte: Either[Throwable, Array[Byte]] =
      Par.run(service)(par)

    println("Par.run(service)(par) gives byte array or throws error")

    eitherArrByte match {
      case Left(throwable) => throw throwable
      case Right(arrByte) =>
        arrByte.toList.foreach((b: Byte) => printf("0x%02X\n", b))

    }
    service.shutdown()

  }
}

object MonolithicLoopRead {
  import IO3._
  import fpinscala.iomonad.Monad
  import fpinscala.laziness.Stream
  import fpinscala.parallelism.Nonblocking.Par

  def fahrenheitToCelsius(fah: Double): Double =
    (fah - 32.0)/1.8

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


