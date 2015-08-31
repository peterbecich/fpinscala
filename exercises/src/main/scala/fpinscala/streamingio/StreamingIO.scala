package fpinscala.streamingio

//                        IO aka IO3.Free[Par,_]
import fpinscala.iomonad.{IO,TailRec,Monad,Free,unsafePerformIO}
import scala.language.higherKinds
import scala.language.postfixOps
import scala.language.implicitConversions

import scala.collection.immutable.{Stream => CollectionStream}
import fpinscala.laziness.{Stream => FPStream}


object GeneralizedStreamTransducers {

  /*

   Our generalized process type is parameterized on the protocol used for
   communicating with the driver. This works similarly to the `IO` type
   we defined in chapter 13. The `Await` constructor emits a request of
   type `F[A]`, and receives a response of type `Either[Throwable,A]`:

   trait Process[F,A]
   case class Await[F[_],A,O](
   req: F[A],
   recv: Either[Throwable,A] => Process[F,O]) extends Process[F,O]
   case class Halt[F[_],O](err: Throwable) extends Process[F,O]
   case class Emit[F[_],O](head: O, tail: Process[F,O]) extends Process[F,O]

   The `Await` constructor may now receive a successful result or an error.

   The `Halt` constructor now has a _reason_ for termination, which may be
   either normal termination indicated by the special exception `End`,
   forceful terimation, indicated by the special exception `Kill`,
   or some other error.

   We'll use the improved `Await` and `Halt` cases together to ensure
   that all resources get released, even in the event of exceptions.

   */

  import fpinscala.iomonad.IO3
  import fpinscala.iomonad.Task
  // translate an IO into a Task
  // we have MonadCatch[Task] already

  val ioMonad = new MonadCatch[IO] {
    //      val monadWithoutCatch = IO3.freeMonad[IO]
    def unit[A](a: => A): IO[A] = IO(a) // defined in package object iomonad
                                        //monadWithoutCatch.unit(a)
    def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] =
      a.flatMap(f)
    //monadWithoutCatch.flatMap(a)(f)

    /* recall IO[A] = Free[Par,A]
     IO does not have built-in containment or handling of failure.
     It can only by handled by adding these features to type A,
     i.e. Option[A]
     */
    def attempt[A](a: IO[A]): IO[Either[Throwable,A]] =
      a.map((a: A) => Right(a))

    def fail[A](t: Throwable): IO[A] = throw t
    // not graceful!  note this eludes the typechecker
  }


  trait Process[F[_],O] {
    import Process._

    /*
     * Many of the same operations can be defined for this generalized
     * `Process` type, regardless of the choice of `F`.
     */

    def map[O2](f: O => O2): Process[F,O2] = this match {
      case Await(req,recv) =>
        Await(req, recv andThen (_ map f))
      case Emit(h, t) => Try { Emit(f(h), t map f) }
      case Halt(err) => Halt(err)
    }

    def ++(p: => Process[F,O]): Process[F,O] =
      this.onHalt {
        case End => Try(p) // we consult `p` only on normal termination
        case err => Halt(err)
      }

    /*
     * Like `++`, but _always_ runs `p`, even if `this` halts with an error.
     */
    def onComplete(p: => Process[F,O]): Process[F,O] =
      this.onHalt {
        case End => p.asFinalizer
        case err => p.asFinalizer ++ Halt(err) // we always run `p`, but preserve any errors
      }

    def asFinalizer: Process[F,O] = this match {
      case Emit(h, t) => Emit(h, t.asFinalizer)
      case Halt(e) => Halt(e)
      case Await(req,recv) => await(req) {
        case Left(Kill) => this.asFinalizer
        case x => recv(x)
      }
    }

    def onHalt(f: Throwable => Process[F,O]): Process[F,O] = this match {
      case Halt(e) => Try(f(e))
      case Emit(h, t) => Emit(h, t.onHalt(f))
      case Await(req,recv) => Await(req, recv andThen (_.onHalt(f)))
    }

    /*
     * Anywhere we _call_ `f`, we catch exceptions and convert them to `Halt`.
     * See the helper function `Try` defined below.
     */
    def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] =
      this match {
        case Halt(err: Throwable) =>
          Halt(err)
        case Emit(o: O, t: Process[F,O]) =>
          Try(f(o)) ++ t.flatMap(f)
        case Await(
          req: F[_],
          recv: Function1[Either[Throwable,_], Process[F,O]]
        ) => Await(req, recv andThen (_ flatMap f))
      }

    def repeat: Process[F,O] =
      this ++ this.repeat

    def repeatNonempty: Process[F,O] = {
      val cycle = (this.map(o => Some(o): Option[O]) ++ emit(None)).repeat
      // cut off the cycle when we see two `None` values in a row, as this
      // implies `this` has produced no values during an iteration
      val trimmed = cycle |> window2 |> (takeWhile {
        case (Some(None), None) => false
        case _ => true
      })
      trimmed.map(_._2).flatMap {
        case None => Halt(End)
        case Some(o) => emit(o)
      }
    }


    //with IO3.freeMonad[IO]

    /*
     Exercise 10: This function is defined only if given a `MonadCatch[F]`.
     Unlike the simple `runLog` interpreter defined in the companion object
     below, this is not tail recursive and responsibility for stack safety
     is placed on the `Monad` instance.

     This will replace the other runLog definition, provided a monad
     MonadCatch[IO] ---
       runLog(implicit monadCatch: MonadCatch[IO]): IO[IndexedSeq[O]]

     How about a simpler example... a Par that produces output
       runLog(implicit monadCatch: MonadCatch[Par]): Par[IndexedSeq[O]]

     Other examples than IO will make more sense after understanding
     the possibilities of this parametrized Process.
     " In order to make Process extensible, we’ll parameterize on
     the protocol used for issuing requests of the driver. "
     */
    def runLog(implicit monadCatch: MonadCatch[F]): F[IndexedSeq[O]] =
      this match {
        case Await(
          req: F[_],
          recv: Function1[Either[Throwable,_],Process[F,O]]
        ) => {
          val attempted = monadCatch.attempt(req) // F[Either[Throwable,_]]
          val fNext = monadCatch.map(attempted){
            (either: Either[Throwable,_])=> recv(either)
          } // F[Process[F,O]]
          //next.runLog(monadCatch): F[IndexedSeq[O]]
          val fIndexedSeq = monadCatch.flatMap(fNext){
            (proc: Process[F,O]) => proc.runLog(monadCatch)
          }
          fIndexedSeq
        }
        case Emit(h, t) => {
          // merge h: O and IndexedSeq[O]
          val fO: F[O] = monadCatch.unit(h)
          val fIndexedSeqO: F[IndexedSeq[O]] = t.runLog(monadCatch)
          val merged = monadCatch.map2(fO,fIndexedSeqO){
            (o: O, iso: IndexedSeq[O]) => iso :+ o
          }
          merged
        }
        case Halt(err) => {
          throw err // caught by case Await?
          // err match {
          // case End => monadCatch.unit(IndexedSeq[O]())
        }
      }


    /*
     * We define `Process1` as a type alias - see the companion object
     * for `Process` below. Using that, we can then define `|>` once
     * more. The definition is extremely similar to our previous
     * definition. We again use the helper function, `feed`, to take
     * care of the case where `this` is emitting values while `p2`
     * is awaiting these values.
     *
     * The one subtlety is we make sure that if `p2` halts, we
     * `kill` this process, giving it a chance to run any cleanup
     * actions (like closing file handles, etc).
     */
    def |>[O2](p2: Process1[O,O2]): Process[F,O2] = {
      p2 match {
        case Halt(e) => this.kill onHalt { e2 => Halt(e) ++ Halt(e2) }
        case Emit(h, t) => Emit(h, this |> t)
        case Await(req,recv) => this match {
          case Halt(err) => Halt(err) |> recv(Left(err))
          case Emit(h,t) => t |> Try(recv(Right(h)))
          case Await(req0,recv0) => await(req0)(recv0 andThen (_ |> p2))
        }
      }
    }

    @annotation.tailrec
    final def kill[O2]: Process[F,O2] = this match {
      case Await(req,recv) => recv(Left(Kill)).drain.onHalt {
        case Kill => Halt(End) // we convert the `Kill` exception back to normal termination
        case e => Halt(e)
      }
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.kill
    }

    /** Alias for `this |> p2`. */
    def pipe[O2](p2: Process1[O,O2]): Process[F,O2] =
      this |> p2

    final def drain[O2]: Process[F,O2] = this match {
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.drain
      case Await(req,recv) => Await(req, recv andThen (_.drain))
    }

    def filter(f: O => Boolean): Process[F,O] =
      this |> Process.filter(f)

    def take(n: Int): Process[F,O] =
      this |> Process.take(n)

    def once: Process[F,O] = take(1)

    /*
     * Use a `Tee` to interleave or combine the outputs of `this` and
     * `p2`. This can be used for zipping, interleaving, and so forth.
     * Nothing requires that the `Tee` read elements from each
     * `Process` in lockstep. It could read fifty elements from one
     * side, then two elements from the other, then combine or
     * interleave these values in some way, etc.
     *
     * This definition uses two helper functions, `feedL` and `feedR`,
     * which feed the `Tee` in a tail-recursive loop as long as
     * it is awaiting input.
     */
    def tee[O2,O3](p2: Process[F,O2])(t: Tee[O,O2,O3]): Process[F,O3] = {
      t match {
        case Halt(e) => this.kill onComplete p2.kill onComplete Halt(e)
        case Emit(h,t) => Emit(h, (this tee p2)(t))
        case Await(side, recv) => side.get match {
          case Left(isO) => this match {
            case Halt(e) => p2.kill onComplete Halt(e)
            case Emit(o,ot) => (ot tee p2)(Try(recv(Right(o))))
            case Await(reqL, recvL) =>
              await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
          }
          case Right(isO2) => p2 match {
            case Halt(e) => this.kill onComplete Halt(e)
            case Emit(o2,ot) => (this tee ot)(Try(recv(Right(o2))))
            case Await(reqR, recvR) =>
              await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
          }
        }
      }
    }

    def zipWith[O2,O3](p2: Process[F,O2])(f: (O,O2) => O3): Process[F,O3] =
      (this tee p2)(Process.zipWith(f))

    def zip[O2](p2: Process[F,O2]): Process[F,(O,O2)] =
      zipWith(p2)((_,_))

    def to[O2](sink: Sink[F,O]): Process[F,Unit] =
      join { (this zipWith sink)((o,f) => f(o)) }

    def through[O2](p2: Channel[F, O, O2]): Process[F,O2] =
      join { (this zipWith p2)((o,f) => f(o)) }
  }

  object Process {
    case class Await[F[_],A,O](
      req: F[A],
      recv: Either[Throwable,A] => Process[F,O]) extends Process[F,O]

    case class Emit[F[_],O](
      head: O,
      tail: Process[F,O]) extends Process[F,O]

    case class Halt[F[_],O](err: Throwable) extends Process[F,O]

    def emit[F[_],O](
      head: O,
      tail: Process[F,O] = Halt[F,O](End)): Process[F,O] =
      Emit(head, tail)

    def await[F[_],A,O](req: F[A])(recv: Either[Throwable,A] => Process[F,O]): Process[F,O] =
      Await(req, recv)

    import fpinscala.iomonad.Monad

    def monad[F[_]]: Monad[({ type f[x] = Process[F,x]})#f] =
      new Monad[({ type f[x] = Process[F,x]})#f] {
        def unit[O](o: => O): Process[F,O] = emit(o)
        def flatMap[O,O2](p: Process[F,O])(f: O => Process[F,O2]): Process[F,O2] =
          p flatMap f
      }

    // enable monadic syntax for `Process` type
    implicit def toMonadic[F[_],O](a: Process[F,O]) =
      monad[F].toMonadic(a)



    /**
     * Helper function to safely produce `p`, or gracefully halt
     * with an error if an exception is thrown.

     Replace any exception with a Halt.
     */
    def Try[F[_],O](p: => Process[F,O]): Process[F,O] =
      try p
      catch { case e: Throwable => Halt(e) }

    /*
     * Safely produce `p`, or run `cleanup` and halt gracefully with the
     * exception thrown while evaluating `p`.
     */
    def TryOr[F[_],O](p: => Process[F,O])(cleanup: Process[F,O]): Process[F,O] =
      try p
      catch { case e: Throwable => cleanup ++ Halt(e) }

    /*
     * Safely produce `p`, or run `cleanup` or `fallback` if an exception
     * occurs while evaluating `p`.
     */
    def TryAwait[F[_],O](p: => Process[F,O])(fallback: Process[F,O], cleanup: Process[F,O]): Process[F,O] =
      try p
      catch {
        case End => fallback
        case e: Throwable => cleanup ++ Halt(e)
      }

    /* Our generalized `Process` type can represent sources! */

    import fpinscala.iomonad.IO

    /* Special exception indicating normal termination */
    case object End extends Exception

    /* Special exception indicating forceful termination */
    case object Kill extends Exception

    /*
     * A `Process[F,O]` where `F` is a monad like `IO` can be thought of
     * as a source.
     */

    /*

     type IO[A] = IO3.IO[A]
     def IO[A](a: => A): IO[A] = IO3.IO[A](a)



     * Here is a simple tail recursive function to collect all the
     * output of a `Process[IO,O]`. Notice we are using the fact
     * that `IO` can be `run` to produce either a result or an
     * exception.

     Makes sure that resource, in this case an Executor, is closed.
     */
    def runLog[O](src: Process[IO,O]): IO[IndexedSeq[O]] = IO {
      val E = java.util.concurrent.Executors.newFixedThreadPool(4)
      @annotation.tailrec
      def go(cur: Process[IO,O], acc: IndexedSeq[O]): IndexedSeq[O] =
        cur match {
          case Emit(h,t) => go(t, acc :+ h)
          case Halt(End) => acc
          case Halt(err) => throw err // caught below?
          case Await(req,recv) =>
            val next =
              try recv(Right(fpinscala.iomonad.unsafePerformIO(req)(E)))
              catch { case err: Throwable => recv(Left(err)) }
            go(next, acc)
        }
      try go(src, IndexedSeq())
      finally E.shutdown
    }

    /*
     * We can write a version of collect that works for any `Monad`.
     * See the definition in the body of `Process`.
     */

    import java.io.{BufferedReader,FileReader}
    val p: Process[IO, String] =
      await(IO(new BufferedReader(new FileReader("resources/lines.txt")))) {
        case Right(b) =>
          lazy val next: Process[IO,String] = await(IO(b.readLine)) {
            case Left(e) => await(IO(b.close))(_ => Halt(e))
            case Right(line) => Emit(line, next)
          }
          next
        case Left(e) => Halt(e)
      }

    /*
     * Generic combinator for producing a `Process[IO,O]` from some
     * effectful `O` source. The source is tied to some resource,
     * `R` (like a file handle) that we want to ensure is released.
     * See `lines` below for an example use.
     */
    def resource[R,O](acquire: IO[R])(
      use: R => Process[IO,O])(
      release: R => Process[IO,O]): Process[IO,O] =
      eval(acquire) flatMap { r => use(r).onComplete(release(r)) }

    /*
     * Like `resource`, but `release` is a single `IO` action.
     */
    def resource_[R,O](acquire: IO[R])(
      use: R => Process[IO,O])(
      release: R => IO[Unit]): Process[IO,O] =
      resource(acquire)(use)(release andThen (eval_[IO,Unit,O]))

    /*
     * Create a `Process[IO,O]` from the lines of a file, using
     * the `resource` combinator above to ensure the file is closed
     * when processing the stream of lines is finished.
     */
    def lines(filename: String): Process[IO,String] =
      resource
    { IO(io.Source.fromFile(filename)) }
    { src =>
      lazy val iter = src.getLines // a stateful iterator
      def step = if (iter.hasNext) Some(iter.next) else None
      lazy val lines: Process[IO,String] = eval(IO(step)).flatMap {
        case None => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
    }
    { src => eval_ { IO(src.close) } }

    /* 
     Exercise 11: 
     Implement `eval`, `eval_`, and use these to implement `lines`. 

     similar to val 'p' above
     */
    def eval[F[_],A](a: F[A]): Process[F,A] = await(a){
      (either: Either[Throwable,A]) => either match {
        case Right(a) => {
          lazy val next: Process[F,A] = emit(a)
          next
        }
        case Left(err) => {
          Halt(err)
        }
      }
    }

    /* Evaluate the action purely for its effects. */
    def eval_[F[_],A,B](a: F[A]): Process[F,B] = ???
      // await(a){
      // (either: Either[Throwable,A]) => Halt[F,B](
      

    /* Helper function with better type inference. */
    def evalIO[A](a: IO[A]): Process[IO,A] =
      eval[IO,A](a)




    /*
     * We now have nice, resource safe effectful sources, but we don't
     * have any way to transform them or filter them. Luckily we can
     * still represent the single-input `Process` type we introduced
     * earlier, which we'll now call `Process1`.
     */

    case class Is[I]() {
      sealed trait f[X]
      val Get = new f[I] {}
    }
    def Get[I] = Is[I]().Get

    type Process1[I,O] = Process[Is[I]#f, O]

    /* Some helper functions to improve type inference. */

    def await1[I,O](
      recv: I => Process1[I,O],
      fallback: => Process1[I,O] = halt1[I,O]): Process1[I, O] =
      Await(Get[I], (e: Either[Throwable,I]) => e match {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(i) => Try(recv(i))
      })

    def emit1[I,O](h: O, tl: Process1[I,O] = halt1[I,O]): Process1[I,O] =
      emit(h, tl)

    def halt1[I,O]: Process1[I,O] = Halt[Is[I]#f, O](End)

    def lift[I,O](f: I => O): Process1[I,O] =
      await1[I,O]((i:I) => emit(f(i))) repeat

    def filter[I](f: I => Boolean): Process1[I,I] =
      await1[I,I](i => if (f(i)) emit(i) else halt1) repeat

    // we can define take, takeWhile, and so on as before

    def take[I](n: Int): Process1[I,I] =
      if (n <= 0) halt1
      else await1[I,I](i => emit(i, take(n-1)))

    def takeWhile[I](f: I => Boolean): Process1[I,I] =
      await1(i =>
        if (f(i)) emit(i, takeWhile(f))
        else      halt1)

    def dropWhile[I](f: I => Boolean): Process1[I,I] =
      await1(i =>
        if (f(i)) dropWhile(f)
        else      emit(i,id))

    def id[I]: Process1[I,I] =
      await1((i: I) => emit(i, id))

    def window2[I]: Process1[I,(Option[I],I)] = {
      def go(prev: Option[I]): Process1[I,(Option[I],I)] =
        await1[I,(Option[I],I)](i => emit(prev -> i) ++ go(Some(i)))
      go(None)
    }

    /** Emits `sep` in between each input received. */
    def intersperse[I](sep: I): Process1[I,I] =
      await1[I,I](i => emit1(i) ++ id.flatMap(i => emit1(sep) ++ emit1(i)))

    /*

     We sometimes need to construct a `Process` that will pull values
     from multiple input sources. For instance, suppose we want to
     'zip' together two files, `f1.txt` and `f2.txt`, combining
     corresponding lines in some way. Using the same trick we used for
     `Process1`, we can create a two-input `Process` which can request
     values from either the 'left' stream or the 'right' stream. We'll
     call this a `Tee`, after the letter 'T', which looks like a
     little diagram of two inputs being combined into one output.

     Note that MonadCatch[T] is not defined.  Cannot use runLog?
     */

    case class T[I,I2]() {
      sealed trait f[X] { def get: Either[I => X, I2 => X] }
      val L = new f[I] { def get = Left(identity) }
      val R = new f[I2] { def get = Right(identity) }
    }
    def L[I,I2] = T[I,I2]().L
    def R[I,I2] = T[I,I2]().R

    type Tee[I,I2,O] = Process[T[I,I2]#f, O]

    /* Again some helper functions to improve type inference. */

    def haltT[I,I2,O]: Tee[I,I2,O] =
      Halt[T[I,I2]#f,O](End)

    def awaitL[I,I2,O](recv: I => Tee[I,I2,O],
      fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
      await[T[I,I2]#f,I,O](L) {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(a) => Try(recv(a))
      }

    def awaitR[I,I2,O](recv: I2 => Tee[I,I2,O],
      fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
      await[T[I,I2]#f,I2,O](R) {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(a) => Try(recv(a))
      }

    def emitT[I,I2,O](h: O, tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
      emit(h, tl)

    def zipWith[I,I2,O](f: (I,I2) => O): Tee[I,I2,O] =
      awaitL[I,I2,O](i  =>
        awaitR        (i2 => emitT(f(i,i2)))) repeat

    def zip[I,I2]: Tee[I,I2,(I,I2)] = zipWith((_,_))

    /* Ignores all input from left. */
    def passR[I,I2]: Tee[I,I2,I2] = awaitR(emitT(_, passR))

    /* Ignores input from the right. */
    def passL[I,I2]: Tee[I,I2,I] = awaitL(emitT(_, passL))

    /* Alternate pulling values from the left and the right inputs. */
    def interleaveT[I]: Tee[I,I,I] =
      awaitL[I,I,I](i =>
        awaitR       (i2 => emitT(i) ++ emitT(i2))) repeat

    /*

     Our `Process` type can also represent effectful sinks (like a file).
     A `Sink` is simply a source of effectful functions! See the
     definition of `to` in `Process` for an example of how to feed a
     `Process` to a `Sink`.

     */

    type Sink[F[_],O] = Process[F, O => Process[F,Unit]]

    import java.io.FileWriter

    /* A `Sink` which writes input strings to the given file. */
    def fileW(file: String, append: Boolean = false): Sink[IO,String] =
      resource[FileWriter, String => Process[IO,Unit]]
    { IO { new FileWriter(file, append) }}
    { w => constant { (s: String) => eval[IO,Unit](IO(w.write(s))) }}
    { w => eval_(IO(w.close)) }

    /* The infinite, constant stream. */
    def constant[A](a: A): Process[IO,A] =
      eval(IO(a)).flatMap { a => Emit(a, constant(a)) }

    /* Exercise 12: Implement `join`. Notice this is the standard monadic combinator! */
    // use monad
    def join[F[_],A](p: Process[F,Process[F,A]]): Process[F,A] = {
      val md = monad[F]
      md.join(p)
    }
      // p.flatMap {
      //   (procFA: Process[F,A]) => procFA
      // }

    /*
     * An example use of the combinators we have so far: incrementally
     * convert the lines of a file from fahrenheit to celsius.
     */

    import fpinscala.iomonad.IO0.fahrenheitToCelsius

    val converter: Process[IO,Unit] =
      lines("fahrenheit.txt").
        filter(line => !line.startsWith("#") && !line.trim.isEmpty).
        map(line => fahrenheitToCelsius(line.toDouble).toString).
        pipe(intersperse("\n")).
        to(fileW("celsius.txt")).
        drain

    /*

     More generally, we can feed a `Process` through an effectful
     channel which returns a value other than `Unit`.

     */

    type Channel[F[_],I,O] = Process[F, I => Process[F,O]]

    /*
     * Here is an example, a JDBC query runner which returns the
     * stream of rows from the result set of the query. We have
     * the channel take a `Connection => PreparedStatement` as
     * input, so code that uses this channel does not need to be
     * responsible for knowing how to obtain a `Connection`.
     */
    import java.sql.{Connection, PreparedStatement, ResultSet}

    def query(conn: IO[Connection]):
        Channel[IO, Connection => PreparedStatement, Map[String,Any]] =
      resource_
    { conn }
    { conn => constant { (q: Connection => PreparedStatement) =>
      resource_
      { IO {
        val rs = q(conn).executeQuery
        val ncols = rs.getMetaData.getColumnCount
        val cols = (1 to ncols).map(rs.getMetaData.getColumnName)
        (rs, cols)
      }}
      { case (rs, cols) =>
        def step =
          if (!rs.next) None
          else Some(cols.map(c => (c, rs.getObject(c): Any)).toMap)
        lazy val rows: Process[IO,Map[String,Any]] =
          eval(IO(step)).flatMap {
            case None => Halt(End)
            case Some(row) => Emit(row, rows)
          }
        rows
      }
      { p => IO { p._1.close } } // close the ResultSet
    }}
    { c => IO(c.close) }

    /*
     * We can allocate resources dynamically when defining a `Process`.
     * As an example, this program reads a list of filenames to process
     * _from another file_, opening each file, processing it and closing
     * it promptly.
     */

    val convertAll: Process[IO,Unit] = (for {
      out <- fileW("celsius.txt").once
      file <- lines("fahrenheits.txt")
      _ <- lines(file).
      map(line => fahrenheitToCelsius(line.toDouble)).
      flatMap(celsius => out(celsius.toString))
    } yield ()) drain

    /*
     * Just by switching the order of the `flatMap` calls, we can output
     * to multiple files.
     */
    val convertMultisink: Process[IO,Unit] = (for {
      file <- lines("fahrenheits.txt")
      _ <- lines(file).
      map(line => fahrenheitToCelsius(line.toDouble)).
      map(_ toString).
      to(fileW(file + ".celsius"))
    } yield ()) drain

    /*
     * We can attach filters or other transformations at any point in the
     * program, for example:
     */
    val convertMultisink2: Process[IO,Unit] = (for {
      file <- lines("fahrenheits.txt")
      _ <- lines(file).
      filter(!_.startsWith("#")).
      map(line => fahrenheitToCelsius(line.toDouble)).
      filter(_ > 0). // ignore below zero temperatures
      map(_ toString).
      to(fileW(file + ".celsius"))
    } yield ()) drain
  }
}

object ProcessTest extends App {
  import GeneralizedStreamTransducers._
  import fpinscala.iomonad.IO
  import Process._

  val p = eval(IO { println("woot"); 1 }).repeat
  val p2 = eval(IO { println("cleanup"); 2 } ).onHalt {
    case Kill => println { "cleanup was killed, instead of bring run" }; Halt(Kill)
    case e => Halt(e)
  }

  println {
    Process.runLog {
      p2.onComplete(p2).onComplete(p2).take(1).take(1)
    }
  }
  println { Process.runLog(converter) }
  // println { Process.collect(Process.convertAll) }
}


object GeneralizedStreamTransducerTests extends App {
  import GeneralizedStreamTransducers._
  import fpinscala.iomonad.IO
  import fpinscala.iomonad.IO3

  import Process._
  import fpinscala.parallelism.Nonblocking.Par

  // val numberFile = new java.io.File("/home/peterbecich/scala/fpinscala/exercises/src/main/scala/fpinscala/streamingio/numbers.txt")

  import java.io.{BufferedReader,FileReader}
  // can't think of a way to turn p into Process[Task,String]
  val p: Process[IO, String] =
    await(IO(new BufferedReader(new FileReader("resources/numbers.txt")))) {
      case Right(b) =>
        lazy val next: Process[IO,String] = await(IO(b.readLine)) {
          case Left(e) => await(IO(b.close))(_ => Halt(e))
          case Right(line) => Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }

  // val numbersOut = runLog(p)

  // println(numbersOut)

  val io = p.runLog(ioMonad)
  println("IO to be run:")
  println(io)

  // later, merge Monad in IOMonad package with Monad in Monads package
  val par = IO3.run(io)(IO3.parMonad)

  println("par to be run:")
  println(par)

  import java.util.concurrent.ExecutorService
  import java.util.concurrent.Executors

  val service = Executors.newFixedThreadPool(4)

  val numbers = Par.run(service)(par)
  println("numbers:")
  println(numbers)

  service.shutdown()
}


