package fpinscala.streamingio

//                        IO aka IO3.Free[Par,_]
import fpinscala.iomonad.{IO,TailRec,Monad,Free,unsafePerformIO}
import scala.language.higherKinds
import scala.language.postfixOps
import scala.language.implicitConversions

import scala.collection.immutable.{Stream => CollectionStream}
import fpinscala.laziness.{Stream => FPStream}


object ImperativeAndLazyIO {

  /*

   We are going to consider various approaches to the simple task of
   checking whether a file contains more than 40,000 lines.

   Our first implementation is an imperative implementation, embedded
   into `IO`.
   */

  import java.io._

  def linesGt40k(filename: String): IO[Boolean] = IO {
    // There are a number of convenience functions in scala.io.Source
    // for reading from external sources such as files.
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      // Obtain a stateful iterator from the Source
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next // has side effect of advancing to next element
        count += 1
      }
      count > 40000
    }
    finally src.close
  }

  /*

   The above code is rather low-level, and it's not compositional,
   either. Consider the following scenarios:

   * Check whether the number of _nonempty_ lines in the file exceeds
   40,000
   * Find a line index before 40,000 where the first letter of
   consecutive lines spells out `"abracadabra"`.

   We cannot just compose our existing implementation with some
   other combinator(s) to implement these tasks. Our implementation is
   a monolithic loop, and we must modify this loop directly if we want
   to change its behavior.

   Now imagine if we had a `CollectionStream[String]` for the lines of the file
   and we could assemble functionality using all the `CollectionStream` functions
   we know and love.
   */

  object Examples {
    val lines: CollectionStream[String] = sys.error("defined elsewhere")
    val ex1 = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)
  }

  /*

   Could we actually write the above? Not quite. We could 'cheat' and
   return an `IO[CollectionStream[String]]` representing the lines of a file:

   */

  def lines(filename: String): IO[CollectionStream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; CollectionStream.empty }
  }
  /*

   This is called _lazy I/O_, and it's problematic for a number of
   reasons, discussed in the book text. However, it would be nice to
   recover the same high-level, compositional style we are used to
   from our use of `List` and `Stream`.

   */
}

object SimpleStreamTransducers {

  /*

   We now introduce a type, `Process`, representing pure, single-input
   stream transducers. It can be in of three states - it can be
   emitting a value to the output (`Emit`), reading a value from its
   input (`Await`) or signaling termination via `Halt`.

   */

  sealed trait Process[I,O] {
    import Process._

    /*
     * A `Process[I,O]` can be used to transform a `Stream[I]` to a
     * `Stream[O]`.
     */
    // def apply(s: CollectionStream[I]): CollectionStream[O] = this match {
    //   case Halt() => CollectionStream()
    //   case Await(recv) => s match {
    //     case h #:: t => recv(Some(h))(t)
    //     case xs => recv(None)(xs) // Stream is empty
    //   }
    //   case Emit(h,t) => h #:: t(s)
    // }


    /*
     Path trace for understanding.

     val nums = Stream(5,Stream(6,Stream(7,...)))
     val passThru: Process[I,I] = Await {
       opI: Option[I] => 
       opI match {
         case Some(i) => emit(i, passThru)
         case None => Halt[I,I]()
       }
     }
     val out = passThru.apply(nums)
     out = Stream(1,Stream(2,Stream(3,...)))

     trace:

     recv: Option[I] => Process[I,I]

     apply(nums) = {
       nums match {
         case Stream.Cons(h: Int , t: Stream[Int]) => {
           val nextProcess: Process[Int,Int] = recv(h): Emit[Int,Int]
           val streamNode: Stream[Int] = nextProcess.apply(t) <<<< lazy
           streamNode
         }
     }


     process: Emit(5,Await(<function1>))
     process: Emit(6,Await(<function1>))
     process: Emit(7,Await(<function1>))
     process: Emit(8,Await(<function1>))
     process: Emit(9,Await(<function1>))
     process: Emit(10,Await(<function1>))
     process: Emit(11,Await(<function1>))
     process: Emit(12,Await(<function1>))
     .
     .
     .

     
     */

    //@annotation.tailrec
    final def apply(s: FPStream[I]): FPStream[O] = this match {
      case Halt() => FPStream.empty
      case Await(recv: Function1[Option[I],Process[I,O]]) =>
        s match {
          case FPStream.cons(h, t) => {
            val process: Process[I,O] = recv(Some(h))
            //println("process: "+process)
            val stream: FPStream[O] = process.apply(t)
            //println("stream node: "+stream)
            stream
          }
          case xs => recv(None)(xs) // Stream is empty
        }
      case Emit(h: O, t: Process[I,O]) => FPStream.cons(h, t(s))
//                                                         ^^^
// could not optimize @tailrec annotated method apply: it contains a recursive call not in tail position
    }


    /*
     * `Process` can be thought of as a sequence of values of type `O`
     * and many of the operations that would be defined for `List[O]`
     * can be defined for `Process[I,O]`, for instance `map`, `++` and
     * `flatMap`. The definitions are analogous.
     */

    def map[O2](f: O => O2): Process[I,O2] =
      this |> liftOne(f)
    // this match {
    //   case Halt() => Halt()
    //   case Emit(h, t) => Emit(f(h), t map f)
    //   case Await(recv) => Await(recv andThen (_ map f))
    // }
    def ++(p: => Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }
    def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] =
      this match {
        case Halt() => Halt()
        case Emit(h, t) => f(h) ++ t.flatMap(f)
        case Await(recv) => Await(recv andThen (_ flatMap f))
      }

    /*
     * Exercise 5: Implement `|>`. Let the types guide your implementation.
     */

    // this: Process[I,O]
    // |> --- Process[I,O], Process[O,O2] => Process[I,O2]

    /*
     simple composition example

     procA: Process[Int,Int] = liftOne( i => (i%26)+65 )
     procB: Process[Int,Char] = liftOne( i => i.toChar )

     procA \> procB : Process[Int,Char]
     no intermediary stream of Ints between 65 and 90


     */
    def |>[O2](p2: Process[O,O2]): Process[I,O2] = {
      // from answers...
      p2 match {
        case Halt() => Halt()
        case Emit(h,t) => Emit(h, this |> t)
        case Await(f) => this match {
          case Emit(h,t) => t |> f(Some(h))
          case Halt() => Halt() |> f(None)
          case Await(g) => Await((i: Option[I]) => g(i) |> p2)
        }
      }
    }
      // await {(i: I) => {
      //   val streamO: FPStream[O] = this.apply(FPStream(i))
      //   val o2: O2 = p2.apply(streamO)
      // }
      // }: Process[I,O2]


    // (this, p2) match {
      //   case (
      //     emit1 @ Emit(h1: O, t1: Process[I,O]),
      //     emit2 @ Emit(h2: O2, t2: Process[O,O2])
      //   ) => {
      //     emit ( h2, this |> p2 )
      //   }: Process[I,O2]
      //   case (
      //     Emit(h1: O, t1: Process[I,O]),
      //     Await(recv2: Function1[Option[O], Process[O,O2]])
      //   ) => await {(i: I) => {

      //     val procOO2: Process[O,O2] = recv2(Some(h1))
      //   }
      //   }: Process[I,O2]
      //   case (
      //     Await(recv1: Function1[Option[I], Process[I,O]]),
      //     _
      //   ) => await {(i: I) => {
      //     val procIO: Process[I,O] = recv1(i)
      //     procIO |> p2
      //   }
      //   }: Process[I,O2]
      //   case (
      //     halt1: Halt[I,O],
      //     _
      //   ) => {
      //     Halt[I,O2]()
      //   }: Process[I,O2]
      // }


    /*
     * Feed `in` to this `Process`. Uses a tail recursive loop as long
     * as `this` is in the `Await` state.
     */
    def feed(in: Seq[I]): Process[I,O] = {
      @annotation.tailrec
      def go(in: Seq[I], cur: Process[I,O]): Process[I,O] =
        cur match {
          case Halt() => Halt()
          case Await(recv) =>
            if (in.nonEmpty) go(in.tail, recv(Some(in.head)))
            else cur
          case Emit(h, t) => Emit(h, t.feed(in))
        }
      go(in, this)
    }


    /*
     * See `Process.lift` for a typical repeating `Process`
     * definition expressed with explicit recursion.
     */

    /*
     * `Process` definitions can often be expressed without explicit
     * recursion, by repeating some simpler `Process` forever.
     */
    def repeat: Process[I,O] = {
      def go(p: Process[I,O]): Process[I,O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

    def repeatN(n: Int): Process[I,O] = {
      def go(n: Int, p: Process[I,O]): Process[I,O] = p match {
        case Halt() => if (n > 0) go(n-1, this) else Halt()
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(n,recv(i))
        }
        case Emit(h, t) => Emit(h, go(n,t))
      }
      go(n, this)
    }

    /*
     * As an example of `repeat`, see `Process.filter`. We define
     * a convenience function here for composing this `Process`
     * with a `Process` that filters the output type `O`.
     */
    def filter(f: O => Boolean): Process[I,O] =
      this |> Process.filter(f)

    def prefilter(f: I => Boolean): Process[I,O] = {
      Process.filter(f) |> this
    }


    /*
     * Exercise 6: Implement `zipWithIndex`.
      with \>  ?

     compose Process[I,O] and Process[O,(O,Int)]
     cannot use the output (O,Int) to determine
     the next output (O,Int+1)

     Need Process[Int,(Int+1)]

     */
    def zip[O2](other: Process[I,O2]): Process[I,(O,O2)] =
      Process.zip(this, other)

    def zipWithIndex: Process[I,(O,Int)] = {
      // val oInt: Process[O,(O,Int)] =
      //   await[O,(O,Int)] {(o: O) =>
      //     emit[O,(O,Int)]{(o, 0)}
      //   }

      // this |> oInt

      // val incrementer: Process[Int,Int] =
      //   emit( 0, await((i:Int) => i+1) )

      Process.zip(this, count)

    }

    /* Add `p` to the fallback branch of this process */
    def orElse(p: Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Await(recv) => Await {
        case None => p
        case x => recv(x)
      }
      case _ => this
    }
  }

  object Process {

    // listing 15.2
    case class Emit[I,O](
      head: O,
      tail: Process[I,O] = Halt[I,O]())
        extends Process[I,O]
    /*
     Don't think Await is any sort of non-blocking, asynchronous
     process
     */
    case class Await[I,O](
      recv: Option[I] => Process[I,O])
        extends Process[I,O]

    case class Halt[I,O]() extends Process[I,O]

    def emit[I,O](head: O,
      tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Emit(head, tail)

    // Process forms a monad, and we provide monad syntax for it

    import fpinscala.iomonad.Monad

    def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] =
      new Monad[({ type f[x] = Process[I,x]})#f] {
        def unit[O](o: => O): Process[I,O] = emit(o)
        def flatMap[O,O2](p: Process[I,O])(f: O => Process[I,O2]): Process[I,O2] =
          p flatMap f
      }

    // enable monadic syntax for `Process` type
    implicit def toMonadic[I,O](a: Process[I,O]) = monad[I].toMonadic(a)

    /**
     * A helper function to await an element or fall back to another process
     * if there is no input.
     */
    def await[I,O](f: I => Process[I,O],
      fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
      //                     ^^ note default value
      Await[I,O] {
        (opI: Option[I]) => opI match {
          case Some(i) => f(i)
          case None => fallback
        }
      }

    /*
     * We can convert any function `f: I => O` to a `Process[I,O]`. We
     * simply `Await`, then `Emit` the value received, transformed by
     * `f`.
     */
    def liftOne[I,O](f: I => O): Process[I,O] =
      Await {
        (opI: Option[I]) => opI match {
          case Some(i) => emit(f(i))
          case None => Halt()
        }
      }

    def lift[I,O](f: I => O): Process[I,O] =
      liftOne(f).repeat

    /*
     * As an example of `repeat`, here's a definition of `filter` that
     * uses `repeat`.
     */
    def filter[I](f: I => Boolean): Process[I,I] =
      Await[I,I] {
        case Some(i) if f(i) => emit(i)
        case _ => Halt()
      }.repeat

    /*
     * Here's a typical `Process` definition that requires tracking some
     * piece of state (in this case, the running total):
     */
    def sum2: Process[Double,Double] = {
      def go(acc: Double): Process[Double,Double] =
        await(d => emit(d+acc, go(d+acc)))
      go(0.0)
    }

    def sum: Process[Double,Double] = {
      val f: (Double, Double) => (Double, Double) =
        (i: Double, s: Double) => (i+s, i+s)
      loop(0.0)(f)
    }


     


    /*
     * Exercise 1: Implement `take`, `drop`, `takeWhile`, and `dropWhile`.
     */

    // recursive step occurs outside Await... not inside
    // only do one iteration inside Await
    // def take[I](n: Int): Process[I,I] = Await[I,I] {
    //   (opI: Option[I]) => opI match {
    //     case Some(i: I) if n>0 => Emit(i, take(n-1))
    //     case Some(i: I) if n<=0 => Halt[I,I]()
    //     case None => Halt[I,I]()
    //   }
    // }
    def take[I](n: Int): Process[I,I] = Await[I,I] {
      (opI: Option[I]) => opI match {
        case Some(i: I) if n>0 => Emit(i)
        case Some(i: I) if n<=0 => Halt[I,I]()
        case None => Halt[I,I]()
      }
    }.repeatN(n-1)

    // def drop[I](n: Int): Process[I,I] =
    //   if (n>0) await {(opI: Option[I]) =>
    //     drop(n-1)
    //   } else {

    //   }

    def drop[I](n: Int): Process[I,I] = Await[I,I] {
      //println(s"n:$n, process:$this")
      (opI: Option[I]) => opI match {
        case Some(i: I) if n>0 => drop(n-1)
        case Some(i: I) if n<=0 => emit(i, passThru) 
        case None => Halt[I,I]()
      }
    }

    // def passThru[I]: Process[I,I] = Await {
    //   (opI: Option[I]) => opI match {
    //     case Some(i) => emit(i)
    //     case None => Halt[I,I]()
    //   }
    // }

    //def passThru = passThru2
    def passThru[I]: Process[I,I] = Await {
      (opI: Option[I]) => opI match {
        case Some(i) => emit(i, passThru)
        case None => Halt[I,I]()
      }
    }


    def takeWhile[I](f: I => Boolean): Process[I,I] = Await[I,I] {
      (opI: Option[I]) => opI match {
        case Some(i: I) if f(i) => Emit(i, takeWhile(f))
        case Some(i: I) => Halt[I,I]()
        case None => Halt[I,I]()
      }
    }


    def dropWhile[I](f: I => Boolean): Process[I,I] =
      Await[I,I] {
        (opI: Option[I]) => opI match {
          case Some(i: I) if f(i) => dropWhile(f)
            //emit(i, dropWhile(f))
          case Some(i: I) => Emit(i, passThru)
          case None => Halt[I,I]()
        }
      }

    /* The identity `Process`, just repeatedly echos its input. */
    def id[I]: Process[I,I] = lift(identity)

    /*
     * Exercise 2: Implement `count`.
     */
    def count[I]: Process[I,Int] = {
      println("count: "+this)
      val f: (I,Int)=>(Int,Int) =
        (i: I, priorCount: Int) => {
          println(s"count; i: $i  prior: $priorCount")
          (priorCount+1, priorCount+1)
        }
      loop[Int,I,Int](0)(f)
    }

    /* For comparison, here is an explicit recursive implementation. */
    def count2[I]: Process[I,Int] = {
      def go(n: Int): Process[I,Int] =
        await((i: I) => emit(n+1, go(n+1)))
      go(0)
    }

    /*
     * Exercise 3: Implement `mean`.
     */
    def mean: Process[Double,Double] = {
      // use sum and count processes
      // sum / count
      // flawed because this is akin to a product
      // remember trying to implement Stream's 'zip' with 'product'?
      // sum.flatMap{(s: Double) =>
      //   count.map((c: Int) => s/c)
      // }
      val sumAndCountProcess: Process[Double,(Int,Double)] =
        Process.zip(Process.count[Double],Process.sum)
      println("sum and count process")
      println(sumAndCountProcess)
      val meanProcess: Process[Double,Double] =
        sumAndCountProcess.map{(tup: (Int,Double)) =>
          //println("tup: "+tup)
          val s = tup._2
          val c = tup._1
          s/c
        }
      println("mean process")
      println(meanProcess)
      meanProcess

    }.repeat

    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
      await {
        (i: I) => {
          val os: (O,S) = f(i,z)
          os match {
            case (o,s2) => emit(o, loop(s2)(f))
          }
        }
      }

    /* Exercise 4: Implement `sum` and `count` in terms of `loop` */


    /*
     * Exercise 7: Can you think of a generic combinator that would
     * allow for the definition of `mean` in terms of `sum` and
     * `count`?
     */

    def feed[A,B](oa: Option[A])(p: Process[A,B]): Process[A,B] =
      p match {
        case Halt() => p
        case Emit(h,t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)
      }

    /*
     * Exercise 6: Implement `zipWithIndex`.
     *
     * See definition on `Process` above.
     */

    /*
     * Exercise 8: Implement `exists`
     *
     * We choose to emit all intermediate values, and not halt.
     * See `existsResult` below for a trimmed version.
     */
    def exists[I](f: I => Boolean): Process[I,Boolean] = {
      val g: (I, Boolean) => (Boolean,Boolean) =
        (next: I, priorDetection: Boolean) => {
          val e = f(next)
          println(s"next: $next satisfies f: $e")
          if(f(next) || priorDetection) (true,true)
          else (false,false)
        }
      loop(false)(g)
    }
    // should halt at first existing input found
    def exists2[I](f: I => Boolean): Process[I,Boolean] =
      await ( (i: I) =>
        if(f(i)) emit(true, Halt[I,Boolean]())
        else emit(false, exists2(f))
      )
    

    /* Awaits then emits a single value, then halts. */
    def echo[I]: Process[I,I] = await(i => emit(i))

    def skip[I,O]: Process[I,O] = await(i => Halt())
    def ignore[I,O]: Process[I,O] = skip.repeat

    def terminated[I]: Process[I,Option[I]] =
      await((i: I) => emit(Some(i), terminated[I]), emit(None))

    def processFile[A,B](f: java.io.File,
      p: Process[String, A],
      z: B)(g: (B, A) => B): IO[B] = IO {
      @annotation.tailrec
      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
        cur match {
          case Halt() => acc
          case Await(recv) =>
            val next = if (ss.hasNext) recv(Some(ss.next))
            else recv(None)
            go(ss, next, acc)
          case Emit(h, t) => go(ss, t, g(acc, h))
        }
      val s = io.Source.fromFile(f)
      try go(s.getLines, p, z)
      finally s.close
    }

    /*
     * Exercise 9: Write a program that reads degrees fahrenheit as `Double` values from a file,
     * converts each temperature to celsius, and writes results to another file.
     */

    def toCelsius(fahrenheit: Double): Double =
      (5.0 / 9.0) * (fahrenheit - 32.0)


    // add all the values (separated by line) in the file together
    // first version of Process doesn't handle errors
    def sumFile(f: java.io.File): IO[Double] = {
      val cast: String => Double =
        (s: String) => s.toDouble

      val fold: (Double,Double)=>Double =
        (acc,sum)=>acc+sum
      // http://stackoverflow.com/questions/9938098/how-to-check-to-see-if-a-string-is-a-decimal-number-in-scala

      val filter = (s: String) => !s.startsWith("#") &&
        s.forall{(c: Char) => c.isDigit} &&
        !s.isEmpty

      // Process needs to handle possibility of uncastable input string
      // val filtered: Process[String,String] =
      //   Process.passThru2[String].filter(filter)

      val stringToDouble: Process[String,Double] =
        Process.lift((s: String) => s.toDouble)

      // val composed: Process[String,Double] = filtered |> stringToDouble

      val composed: Process[String,Double] =
        stringToDouble.prefilter(filter)

      // val stringToDouble: Process[String,Double] =
      //   await((s: String) =>
      //     try {emit(cast(s))}
      //     catch {
      //       case nfe: NumberFormatException => {
      //         // side effect...
      //         println(nfe)
      //         Halt[String,Double]()
      //       }
      //     }
      //   )//.repeat


      processFile(f, composed, 0.0)(fold)
    }

    // def celsiusFileWriter: IO = IO {
    //   val 
    // }


    def zip[A,B,C](p1: Process[A,B], p2: Process[A,C]): Process[A,(B,C)] = {
      // println("_______________")
      // println("zip:")
      // println(p1)
      // println(p2)
      // println("_______________")
      (p1, p2) match {
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        case (Emit(b, t1), Emit(c, t2)) => Emit((b,c), zip(t1, t2))
        case (Await(recv1), _) =>
          Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
        case (_, Await(recv2)) =>
          Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
      }
    }

  }
}


object StreamingIOTests {
  import SimpleStreamTransducers._
  //import fpinscala.laziness.Stream
  import java.util.concurrent.ExecutorService
  import java.util.concurrent.Executors
  import fpinscala.parallelism.Nonblocking.Par
  import fpinscala.iomonad.IO3


  val service = Executors.newFixedThreadPool(4)


  val streamIncrementing: FPStream[Int] = FPStream.from(0)

  val f = (x: Int) => x*2
  val p: Process[Int, Int] = Process.liftOne(f)

  val evenProcess = Process.filter((x: Int) => x%2==0)
  // current 'take' method only applies to CollectionStream
  //val someEvenNumbersProcess = evenProcess.take(20)


  // modulo.  [65, 90]
  val asciiCodes: Process[Int,Int] = Process.lift((i: Int) => (i%26)+65)
  val toChar: Process[Int,Char] = Process.lift((i: Int) => i.toChar)
  val ascii: Process[Int,Char] = asciiCodes |> toChar

  val numberFile = new java.io.File("resources/numbers.txt")

  val fahrenheitFile = new java.io.File("resources/fahrenheit.txt")
  val numberFileSummed: IO[Double] =
    Process.sumFile(numberFile)

  val flawedNumberFile = new java.io.File("resources/numbers_flawed.txt")
  val flawedNumberFileSummed: IO[Double] =
    Process.sumFile(flawedNumberFile)


  def main(args: Array[String]): Unit = {
    println("naive function")
    println(f)
    println("lifted")
    println(p)
    println("incrementing numbers")
    streamIncrementing.feedback
    println("note the examples below do not compose Processes")

    println("pass through")
    val passedThru = Process.passThru(streamIncrementing)
    passedThru.feedback

    println("drop the first 10 numbers")
    val dropped = Process.drop(10)(streamIncrementing)
    dropped.feedback
    println("some even numbers, larger than 5")
    val streamEven: FPStream[Int] = evenProcess(
        Process.take[Int](20)(streamIncrementing)
      )
    println(streamEven)
    streamEven.feedback

    println("count ints")
    Process.count(streamEven).feedback

    println("count doubles")
    Process.count(streamEven.map(_.toDouble)).feedback

    println("take while _ < 10")
    val lessThanTen =
      Process.takeWhile((x:Int)=>x<10)(streamIncrementing)
    lessThanTen.feedback

/*
Note this counterintuitive result.  The dropping of all numbers 
less than 10 *delays* the process checking for the existence of 20.

next: 0 satisfies f: false
process: Await(<function1>)
next: 1 satisfies f: false
process: Await(<function1>)
next: 2 satisfies f: false
process: Await(<function1>)
next: 3 satisfies f: false
process: Await(<function1>)
next: 4 satisfies f: false
process: Await(<function1>)
next: 5 satisfies f: false
process: Await(<function1>)
next: 6 satisfies f: false
process: Await(<function1>)
next: 7 satisfies f: false
process: Await(<function1>)
next: 8 satisfies f: false
process: Await(<function1>)
next: 9 satisfies f: false
process: Await(<function1>)
next: 10 satisfies f: false
process: Emit((10,false),Await(<function1>))
next: 11 satisfies f: false
process: Emit((11,false),Await(<function1>))
next: 12 satisfies f: false
process: Emit((12,false),Await(<function1>))
next: 13 satisfies f: false
process: Emit((13,false),Await(<function1>))
next: 14 satisfies f: false
process: Emit((14,false),Await(<function1>))
next: 15 satisfies f: false
process: Emit((15,false),Await(<function1>))
next: 16 satisfies f: false
process: Emit((16,false),Await(<function1>))
next: 17 satisfies f: false
process: Emit((17,false),Await(<function1>))
next: 18 satisfies f: false
process: Emit((18,false),Await(<function1>))
next: 19 satisfies f: false
process: Emit((19,false),Await(<function1>))
next: 20 satisfies f: true
process: Emit((20,false),Await(<function1>))
next: 21 satisfies f: false
process: Emit((21,false),Await(<function1>))
next: 22 satisfies f: false
process: Emit((22,false),Await(<function1>))
next: 23 satisfies f: false
process: Emit((23,false),Await(<function1>))
next: 24 satisfies f: false
process: Emit((24,false),Await(<function1>))
next: 25 satisfies f: false
process: Emit((25,false),Await(<function1>))
next: 26 satisfies f: false
process: Emit((26,false),Await(<function1>))
next: 27 satisfies f: false
process: Emit((27,false),Await(<function1>))
next: 28 satisfies f: false
process: Emit((28,false),Await(<function1>))
next: 29 satisfies f: false
process: Emit((29,false),Await(<function1>))
next: 30 satisfies f: false
process: Emit((30,true),Await(<function1>))
next: 31 satisfies f: false
process: Emit((31,true),Await(<function1>))
next: 32 satisfies f: false



 */

  }
}

object RollingMean extends App {
  import SimpleStreamTransducers._

  val streamIncrementing: FPStream[Int] = FPStream.from(0)

  val evenProcess = Process.filter((x: Int) => x%2==0)
  
  val streamEven: FPStream[Int] = evenProcess(
    Process.take[Int](20)(streamIncrementing)
  )

  println("incrementing mean of these:")
  val incMean = Process.mean(streamEven.map((i:Int)=>i.toDouble))
  incMean.feedback



}

object ZipWith extends App {
  import SimpleStreamTransducers._
  val streamIncrementing: FPStream[Int] = FPStream.from(0)
  println("drop while _ < 10")
  val tenAndGreaterProcess =
    Process.dropWhile((x:Int)=>x<10)
  val tenAndGreater = tenAndGreaterProcess.apply(streamIncrementing)
  tenAndGreater.feedback

  println("number 20 exists in Stream")
  val twentyExists: Process[Int,Boolean] = Process.exists(
    (i: Int) => i==20)
  twentyExists(streamIncrementing).feedback
  
  
  println("zip with")
  val tenAndGreaterAndTwentyExists = Process.zip(tenAndGreaterProcess, twentyExists)
  tenAndGreaterAndTwentyExists.apply(streamIncrementing).feedback


  println("ten and greater zipped with count")
  val countZip = Process.zip(tenAndGreaterProcess, Process.count[Int])

  countZip.apply(streamIncrementing).feedback


}

object ComposingProcesses extends App {
  import SimpleStreamTransducers._
  val streamIncrementing: FPStream[Int] = FPStream.from(0)
  val asciiCodes: Process[Int,Int] = Process.lift((i: Int) => (i%26)+65)
  val toChar: Process[Int,Char] = Process.lift((i: Int) => i.toChar)

  println("composing processes (|>)")
  println("ASCII codes")
  asciiCodes(streamIncrementing).feedback
  println("to Char")
  toChar(streamIncrementing).feedback
  println("ASCII codes |> to Char")
  (asciiCodes |> toChar)(streamIncrementing).feedback



}

object SummingFile extends App {
  import SimpleStreamTransducers._
  //import fpinscala.laziness.Stream
  import java.util.concurrent.ExecutorService
  import java.util.concurrent.Executors
  import fpinscala.parallelism.Nonblocking.Par
  import fpinscala.iomonad.IO3

  val flawedNumberFile = new java.io.File("resources/numbers_flawed.txt")
  val flawedNumberFileSummed: IO[Double] =
    Process.sumFile(flawedNumberFile)

  val numberFile = new java.io.File("resources/numbers.txt")

  val fahrenheitFile = new java.io.File("resources/fahrenheit.txt")
  val numberFileSummed: IO[Double] =
    Process.sumFile(numberFile)

  val service = Executors.newFixedThreadPool(4)

  println("summing the numbers in a file")
  val par =
    IO3.run(numberFileSummed)(IO3.parMonad)

  val summed = Par.run(service)(par)

  println("summed")
  println(summed)

  println("with a line in the file that cannot be cast to Double")

  // "abc"
  // java.lang.NumberFormatException: For input string: "abc"

  val errorPar =
    IO3.run(flawedNumberFileSummed)(IO3.parMonad)
  val errorSum = Par.run(service)(errorPar)

  println(errorSum)

  service.shutdown()

}

object InfiniteStreamingIOTest {
  import SimpleStreamTransducers._

  val emitOne: Process[Unit,Int] = Process.liftOne((_:Unit)=>1)
  val emitOneRepeat: Process[Unit,Int] = Process.lift((_:Unit)=>1)
  //val emitOneForever: Process[Unit,Int] = emitOne.repeat

  def main(args: Array[String]): Unit = {
    println("note that a Process can limit the length of the output stream")
    println("but cannot create an output stream of longer length than the input stream.")
    println("Note wording in section 15.3: Process can request to Emit values multiple times.")
    println("Process.apply is not so much the 'driver' as the input Stream is.  The input Stream can limit the ouput of the Process, rejecting the 'request'.")
    println("infinite stream of Units for input to Process")
    val infiniteStream = FPStream.constant(())
    infiniteStream.feedback
    val stream = emitOne.apply(infiniteStream)
    println("emit One; Process.liftOne")
    stream.feedback

    val stream2 = emitOneRepeat.apply(infiniteStream)
    println("emit One repeat; Process.lift")
    stream2.feedback

  }
}


