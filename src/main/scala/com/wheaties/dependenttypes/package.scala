package com.wheaties

import scala.collection.{GenTraversableLike, GenTraversableOnce}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import shapeless.test.illTyped

/**
  *
  */
package object dependenttypes {

  /**
    * Slide: http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/1/2
    */
  trait Foo{
    class Bar
    def doNothing(b: Bar){}
  }
  val f1 = new Foo{}
  val b1 = new f1.Bar()
  val f2 = new Foo{}
  val b2 = new f2.Bar()

  f1.doNothing(b1) //fine
  illTyped("f1.doNothing(b2)") //won't compile because type member Bar is different in each instance of Foo
  f2.doNothing(b2) //fine


  /**
    *
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/1/3
    */
  //package scala.collection.generic

  trait IsTraversableOnce[Repr]{
    type A
    val conversion: Repr => GenTraversableOnce[A]
  }

  trait IsTraversableLike[Repr]{
    type A
    def conversion(repr: Repr): GenTraversableLike[A, Repr]
  }

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/2
    */

  trait Inner[F]{
    type T
  }


  object Inner{
    def apply[F](implicit inner: Inner[F]) = inner

    implicit def mk[F[_], A] = new Inner[F[A]]{
      type T = A
    }
  }

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/2/1
    */
  trait IsFuture[F]{
    type T

    def apply(f: F): Future[T]
  }

  object IsFuture{
    def apply[F](implicit isf: IsFuture[F]) = isf

    implicit def mk[A] = new IsFuture[Future[A]]{
      type T = A

      def apply(f: Future[A]): Future[A] = f
    }
  }

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/2/2
    */

  def logResult[Thing](thing: Thing)
                      (implicit isf: IsFuture[Thing]): Future[isf.T] =
    isf(thing) map{ x =>
      /*log info*/ s"I got a result of $x"
      x
    }

  /**
    * TAKE AWAY

    * 1. Use companion objects to hide implicit creation and any other boilerplate that might be needed to
    *     ultimately construct the dependently typed object.
    *
    * 2. Expose the constructed type classes using an apply that returns the implicit sought.
    *
    * 3. Dependent types can used as type parameters
    *
    * 4. Dependent types allow us to safely cross type boundaries.
    */
}
