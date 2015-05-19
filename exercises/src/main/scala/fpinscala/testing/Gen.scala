package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import fpinscala.monads.Functor
import fpinscala.monads.Monad
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

//case class Prop(run: ((PropTypes.TestCases, RNG) => PropTypes.Result))

case class Prop(run: ((TestCases, RNG) => Result)) {
  def check(tc: TestCases, rng: RNG): 
      Either[(FailedCase, SuccessCount), SuccessCount] = {
    val result: Result = run((tc, rng))
    val etr

  }
  def &&(otherProp: Prop): Prop = {
    val p2Check:
        Either[(FailedCase, SuccessCount), SuccessCount] =
    (check, otherProp.check) match {
      case (Left((failedCase1, succCount1)),
        Left((failedCase2, succCount2))) => {
        Left((failedCase1+failedCase2, succCount1+succCount2))
      }
      case (Right(succCount1),
        Left((failedCase2, succCount2))) => {
        Left((failedCase2, succCount1+succCount2))
      }
      case (Left((failedCase1, succCount1)),
        Right(succCount2)) => {
        Left((failedCase1, succCount1+succCount2))
      }
      case (Right(succCount1), Right(succCount2)) => {
        Right(succCount1 + succCount2)
      }
    }

    val mergedProps = new Prop {
      def check:
          Either[(FailedCase, SuccessCount), SuccessCount] =
        p2Check
    }
    mergedProps
  }
}

object Prop {
  //def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case object Failed extends Result {
    def isFalsified = true
  }
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
 case class State[S,A](run: S => (AS))
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

  // def below considered to be 'apply'
  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)({(a: A, b: B) => {
      (a, b): Tuple2[A, B]
    }}: (A, B) => Tuple2[A, B]
    ): Gen[Tuple2[A, B]]
  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }


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

  val ES: ExecutorService = Executors.newCachedThreadPool
  /*
   In this example, Prop.forAll takes in a Gen that will only ever
   generate one value -- Par.unit(1).
   How will Prop.forAll handle a generator that generates many Pars?
   */
  val parAdditionProp = Prop.forAll(Gen.unit(Par.unit(1))){
    (pi: Par[Int]) => {
      Par.map(pi)(_ + 1)(ES).get == Par.unit(2)(ES).get
    }
  }
  val randomsAboveZeroProp = Prop.forAll(Gen.choose(4, 40)){
    (i: Int) => i > 0
  }


}

// sized generator
case class SGen[+A](forSize: Int => Gen[A]){

}

