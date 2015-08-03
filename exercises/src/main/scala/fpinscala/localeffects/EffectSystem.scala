package fpinscala.effectsystem

import fpinscala.monads._

// https://apocalisp.wordpress.com/2011/03/20/towards-an-effect-system-in-scala-part-1/

object EffectSystem {
  case class World[W]()

  case class ST[S,A](f: World[S] => (World[S],A)) {
    def apply(s: World[S]) = f(s)
    def flatMap[B](g: A => ST[S,B]): ST[S,B] = ST {
      (worldState: World[S]) => f(worldState) match {
        case (ns, a) => g(a)(ns)
      }
    }
    def map[B](g: A => B): ST[S,B] = ST {
      (worldState: World[S]) => f(worldState) match {
        case (ns, a) => (ns, g(a))
      }
    }
  }

  def returnST[S,A](a: => A): ST[S,A] = ST {
    (worldState: World[S]) => (worldState, a)
  }

  case class STRef[S,A](a: A) {
    private var value: A = a

    def read: ST[S,A] = returnST(value)
    def write(a: A): ST[S, STRef[S,A]] = ST {
      (worldState: World[S]) => {
        value = a
        (worldState, this)
      }
    }
    def mod[B](f: A => A): ST[S, STRef[S,A]] =
      read.flatMap(a =>
        write(f(a)).flatMap(v =>
          returnST(v)
        )
      )

  }

  def newVar[A](a: => A) = returnST(STRef(a))

  // def e1[S]: ST[S, STRef[S, Int]] =
  //   newVar(0).flatMap(r =>
  //     r.mod(_+1).flatMap(x =>
  //       returnST(x)
  //     )
  //   )

  def main(args: Array[String]): Unit = {

  }



}
