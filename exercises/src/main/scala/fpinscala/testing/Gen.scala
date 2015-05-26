package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import fpinscala.monads.Functor
import fpinscala.monads.Monad
import Gen._
import Prop._
import fpinscala.laziness.Stream
import java.util.concurrent.{Executors,ExecutorService}

import scala.collection.immutable.{Stream => _}

//case class Prop(run: ((PropTypes.TestCases, RNG) => PropTypes.Result))

case class Prop (
  run: (
    Prop.MaxSize,  // Int
    Prop.TestCases,  // Int
    RNG
  ) => Prop.Result
){


  def &&(otherProp: Prop): Prop = Prop (
    (
      max: Prop.MaxSize,  // Int
      n: Prop.TestCases,  // Int
      rng: RNG
    ) => {
      val thisResult: Prop.Result = this.run(max, n, rng)
      val otherResult: Prop.Result = otherProp.run(max, n, rng)
        //(thisResult, otherResult) match {
        /*
         ^^^ Interesting fluke in formatter:
         tuple above assumed to be application to otherResult,
         and so is indented incorrectly.
         */
      val mergedResult: Prop.Result = (thisResult, otherResult) match {
        case (Passed, Passed) => Passed
        case (falsified1: Falsified, Passed) =>
          falsified1
        case (Passed, falsified2: Falsified) =>
          falsified2
        case (
          Falsified(failure1, successes1),
          Falsified(failure2, successes2)
        ) => Falsified(failure1+", "+failure2, successes1+successes2)
      }
      mergedResult
    }
  )
  def ||(otherProp: Prop): Prop = Prop (
    (
      max: Prop.MaxSize,  // Int
      n: Prop.TestCases,  // Int
      rng: RNG
    ) => {
      val thisResult: Prop.Result = this.run(max, n, rng)
      val otherResult: Prop.Result = otherProp.run(max, n, rng)
      val mergedResult: Prop.Result = (thisResult, otherResult) match {
        case (Passed, Passed) => Passed
        case (falsified1: Falsified, Passed) =>
          Passed
        case (Passed, falsified2: Falsified) =>
          Passed
        case (
          Falsified(failure1, successes1),
          Falsified(failure2, successes2)
        ) => Falsified(failure1+", "+failure2, successes1+successes2)
      }
      mergedResult
    }
  )

}

object Prop {
  // this method and case class Prop's run method both have the same name,
  // but are differentiated by their arguments, I think.


  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis().toLong)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Prop.Falsified(msg, n) =>
        println(s"Falsified after $n passed tests: \n $msg")
      case Prop.Passed => println(s"Passed $testCases tests")
    }

  // forALl for Gen[A] from answers
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = {
    val g: (MaxSize, TestCases, RNG) => Result = 
      (max: Int, n: Int, rng: RNG) => {
        val streamA: Stream[A] = 
          randomStream(as)(rng)
        println("Stream[A]")
        streamA.feedback

        val streamAInt: Stream[(A, Int)] =
          streamA.zip(Stream.from(0))
        println("Stream[(A, Int)]")
        streamAInt.feedback

        // 'take' is, I think, makes a lazy stream strict, up to 'n' nodes
        val taken: Stream[(A, Int)] = 
          streamAInt.take(n)
        println("taken")
        taken.feedback

        val streamResult: Stream[Result] = 
          taken.map {(tpl: Tuple2[A,Int]) => {
            val a: A = tpl._1
            val i: Int = tpl._2
            println(i)
            val result: Result = try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
            result
          }: Result
          }: Stream[Result]
        println("Stream[Result]")
        streamResult.feedback

        // val optionAggregatedResult: Option[Result] =
        //   streamResult.find((r: Result) => r match {
        //     case fpinscala.testing.Prop.Passed => false
        //     case fpinscala.testing.Prop.Falsified(
        //       failure: String, successes: Int
        //     ) => {
        //       println(failure)
        //       println("successes: "+successes)
        //       true
        //     }
        //   }
        //   )
        // val aggregatedResult: Result =
        //   optionAggregatedResult.getOrElse(Passed)
          

        // println("Result: "+aggregatedResult)

        // aggregatedResult
        Prop.Passed
      }
  
    Prop(g)
  }



  def forAll[A](sGen: SGen[A])(f: A => Boolean): Prop =
    forAll(sGen(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop (
    (max: MaxSize, n: TestCases, rng: RNG) => {
      val casesPerSize = (n + (max - 1)) / max

      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map {
          (i: Int) => this.forAll(g(i))(f)
        }

      val propResults: Stream[Prop] = props.map {
        (p0: Prop) => Prop { (max, _, rng) =>
          p0.run(max, casesPerSize, rng)
        }}

      val reducedProp: Prop = propResults.toList.reduce {
        (p1: Prop, p2: Prop) => p1.&&(p2)
      }
      reducedProp.run(max, n, rng)
    }
  )


  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(
    failure: FailedCase,
    successes: SuccessCount
  ) extends Result {
    def isFalsified = true
  }

  def randomStream[A](g: Gen[A])(rng0: RNG): Stream[A] =
    Stream.unfold(rng0){
      (rng1: RNG) => {
        val (a, rng2): Tuple2[A, RNG] = g.sample.run(rng1)
        Some((a, rng2))
      }
    }
  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a \n" +
  s"generated an exception: $e \n" +
  s"stack trace: \n ${e.getStackTrace().mkString("\n")}"

}

// object PropTypes {
//   type SuccessCount = Int
//   type FailedCase = String
//   type TestCases = Int

//   sealed trait Result {
//     def isFalsified: Boolean
//   }
//   case object Passed extends Result {
//     def isFalsified = false
//   }
//   case object Failed extends Result {
//     def isFalsified = true
//   }

// }

/*
 case class State[S,A](run: S => (A, S))
 */

// A monad?
// case class Gen[A](sample: State[RNG, A]){
case class Gen[A](sample: State.Rand[A]){
  def map[B](f: A => B): Gen[B] = {
    val newState: State.Rand[B] = this.sample.map(f)
    Gen[B](newState)
  }

  def map2[B,C](gb: Gen[B])(f: (A, B) => C): Gen[C] = {
    this.flatMap({(a: A) => {
      gb.map((b: B) => f(a, b)): Gen[C]}
    }: A => Gen[C]
    ): Gen[C]
  }: Gen[C]

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    // How can this be simplified?
    // Is this equivalent to the meticulous, error-prone "wiring"
    // between states, mentioned here?
    // https://www.youtube.com/watch?v=Jg3Uv_YWJqI
    Gen{
      State{
        (rng0: RNG) => {
          val (a1, rng1): Tuple2[A, RNG] = sample.run(rng0)
          val fOut: Gen[B] = f(a1)
          val (b0, rng2): Tuple2[B, RNG] = fOut.sample.run(rng1)
          (b0, rng2)
        }
      }
    }
  }
  // redundant use of method name, but arguments are different
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    // Int => Gen[List[A]]
    size.flatMap {
      (i: Int) => {
        Gen.listOfN(i, this)
      }
    }
  }

  // 'def **' is the apply counterpart to 'object **.unapply'
  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)({(a: A, b: B) => {
      (a, b): Tuple2[A, B]
    }}: (A, B) => Tuple2[A, B]
    ): Gen[Tuple2[A, B]]

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def unsized: SGen[A] = SGen((i: Int) => this)  // i ignored?

}

object Gen {
  val genMonad = Monad.genMonad

  def unit[A](a: => A): Gen[A] = new Gen(State.unit(a))
  // def boolean: Gen[Boolean] 

  // do it with flatMap
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val ll = List.fill(n)(g)
    // traverse, sequence, or replicate
    // genMonad.sequence(lma: List[Gen[A]])
    genMonad.sequence(ll)
  }
  def _listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val listState = List.fill(n)(g.sample)
    Gen(State.sequence(listState))
  }
  def stringOfLength(n: Int): Gen[String] = {
    val genAsciiInt = choose(0x21, 0x7a)
    val genListInt: Gen[List[Int]] = listOfN(n, genAsciiInt)
    genListInt.map((li: List[Int]) => li.toString())
  }

  // no randomness here
  // Gen is limited to using State[RNG,A]
  // So it's not possible to create a Gen of State[Int, Int],
  // to make a counting generator
  // def counter(start: Int): Gen[Int] = {

  // }
  // generates one integer
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    // use Simple RNG
    // val genInt = Gen(State(RNG.chooseInt(rng)(start, stopExclusive)))
    /*
     type State.Rand[Int] == State[RNG, Int]
     instance of State.Rand[Int] includes method
     run: RNG => (Int, RNG)

     */
    val stateInt: State.Rand[Int] = State {
      (rng: RNG) => RNG.chooseInt(rng)(start, stopExclusive)
    }
    val genInt: Gen[Int] = Gen(stateInt)
    genInt
  }

  // make a generator of incrementing ints
  // def genInt: Gen[Int] = {
  //   val stateInt: State.Rand[Int] = State {
  //     (rng: RNG) => RNG.chooseInt(rng)(start, stopExclusive)
  //   }
  //   Gen(stateInt)
  // }

  def chooseDouble(start: Double, stopExclusive: Double): Gen[Double] = {
    val stateDouble: State.Rand[Double] = State {
      (rng: RNG) => RNG.chooseDouble(rng)(start, stopExclusive)
    }
    val genDouble: Gen[Double] = Gen(stateDouble)
    genDouble
  }


  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    // choose int in [0,2)
    // 0, return g1
    // 1, return g2
    val genInt: Gen[Int] = choose(0, 2)
    val genChosen: Gen[A] = genInt.flatMap {
      (i: Int) => if(i==0) g1 else g2
    }
    genChosen
  }

  // can weights be negative?
  // nowhere stated that weights add up to 1.0
  // lets assume weights are positive...
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val w1: Double = scala.math.abs(g1._2)
    val w2: Double = scala.math.abs(g2._2)
    // val min: Double = scala.math.min(w1, w2)
    // val max: Double = scala.math.max(w1, w2)
    val sum: Double = w1+w2
    val genDouble: Gen[Double] = chooseDouble(0, sum)
    val genChosen: Gen[A] = genDouble.flatMap {
      (d: Double) => if(d<w1) g1._1 else g2._1
    }
    genChosen
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen((size: Int) => {
      // A => List[A]
      val genListA: Gen[List[A]] =
        Gen.listOfN(size, g)
      genListA
    }
    )
  }

  // val ES: ExecutorService = Executors.newCachedThreadPool
  // /*
  //  In this example, Prop.forAll takes in a Gen that will only ever
  //  generate one value -- Par.unit(1).
  //  How will Prop.forAll handle a generator that generates many Pars?
  //  */
  // val parAdditionProp = Prop.forAll(Gen.unit(Par.unit(1))){
  //   (pi: Par[Int]) => {
  //     Par.map(pi)(_ + 1)(ES).get == Par.unit(2)(ES).get
  //   }
  // }
  // val randomsAboveZeroProp = Prop.forAll(Gen.choose(4, 40)){
  //   (i: Int) => i > 0
  // }


}

object PropTests {

  val chooseGenInt: Gen[Int] = Gen.choose(5, 30)
  val simpleRNG: RNG = RNG.Simple(123)

  val randomIntStream: Stream[Int] =
    Prop.randomStream(chooseGenInt)(simpleRNG)

  val chosenIntRangeProp: Prop =
    Prop.forAll(chooseGenInt)((i: Int) => (i>=5 && i<30))

  val simpleProp = Prop(
    (maxSize: Int, testCases: Int, rng: RNG) => Prop.Passed
  )

  def main(args: Array[String]): Unit = {
    val ES: ExecutorService = Executors.newCachedThreadPool

    /*
     big runtime error with Stream.

     [error] (run-main-6) java.lang.StackOverflowError
     java.lang.StackOverflowError
     at fpinscala.laziness.Stream$.from(Stream.scala:275)
     at fpinscala.laziness.Stream$$anonfun$from$2.apply(Stream.scala:275)
     at fpinscala.laziness.Stream$$anonfun$from$2.apply(Stream.scala:275)
     at fpinscala.laziness.Stream$.tail$lzycompute$1(Stream.scala:236)
     at fpinscala.laziness.Stream$.fpinscala$laziness$Stream$$tail$1(Stream.scala:236)
     at fpinscala.laziness.Stream$$anonfun$cons$2.apply(Stream.scala:237)
     at fpinscala.laziness.Stream$$anonfun$cons$2.apply(Stream.scala:237)
     at fpinscala.laziness.Stream$$anonfun$foldRight$1.apply(Stream.scala:15)
     at fpinscala.laziness.Stream$class.g$2(Stream.scala:181)
     at fpinscala.laziness.Stream$$anonfun$flatMap$2.apply(Stream.scala:182)
     at fpinscala.laziness.Stream$$anonfun$flatMap$2.apply(Stream.scala:182)
     at fpinscala.laziness.Stream$class.foldRight(Stream.scala:15)
     at fpinscala.laziness.Cons.foldRight(Stream.scala:231)
     at 


     */

    /*
     These don't exist *until* main is run!
     One of the differences between a def and val.

     scala> PropTests.main(Array[String]()).chooseGenInt
     <console>:9: error: value chooseGenInt is not a member of Unit
     PropTests.main(Array[String]()).chooseGenInt
                                     ^
     */

    // val chooseGenInt: Gen[Int] = Gen.choose(5, 30)
    // val simpleRNG: RNG = RNG.Simple(123)

    // val randomIntStream: Stream[Int] = 
    //   Prop.randomStream(chooseGenInt)(simpleRNG)
    println("random int stream")
    println(randomIntStream.toListFinite(15))

    // val chosenIntRangeProp: Prop = 
    //   Prop.forAll(chooseGenInt)((i: Int) => (i>=5 && i<30))

    // runtime error
    Prop.run(chosenIntRangeProp)

    // runtime error
    //val result: Result = chosenIntRangeProp.run(5,5,simpleRNG)


    /*
     In this example, Prop.forAll takes in a Gen that will only ever
     generate one value -- Par.unit(1).
     How will Prop.forAll handle a generator that generates many Pars?
     */
    // val parAdditionProp: Prop = Prop.forAll(Gen.unit(Par.unit(1))){
    //   (pi: Par[Int]) => {
    //     Par.map(pi)(_ + 1)(ES).get == Par.unit(2)(ES).get
    //   }
    // }
    // Prop.run(parAdditionProp)

    // val randomsAboveZeroProp = Prop.forAll(Gen.choose(4, 40)){
    //   (i: Int) => i > 0
    // }
    // Prop.run(randomsAboveZeroProp)

    // val smallInt: Gen[Int] = Gen.choose(-10,10)
    // val smallIntProp: Prop = forAll(Gen.listOf(smallInt)) {
    //   (la: List[Int]) => {
    //     val max = la.max
    //     !la.exists((i: Int) => i>max)
    //   }
    // }
    // Prop.run(smallIntProp)

    ES.shutdown()
  }


}

// sized generator

/*
 covariant type A occurs in invariant position in type 
 => Int => fpinscala.testing.Gen[A] of value g
 case class SGen[+A](g: Int => Gen[A]){...
                     ^
 Given A >: B  (Number >: Double)
 SGen[A] >: SGen[B]  (SGen[Number] >: SGen[Double])

 but in function g
 Gen[A] and Gen[B] have no relation
 or
 (Int => Gen[A]) and (Int => Gen[B]) have no relation
 */
// case class SGen[+A](g: Int => Gen[A]){


case class SGen[A](g: Int => Gen[A]){
// want covariance
//case class SGen[A >: B](g: Int => Gen[A]){
  // from answers...
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(g andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(g andThen (_ flatMap f))

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))


}

