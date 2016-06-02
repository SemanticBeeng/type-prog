package com.wheaties

import shapeless.ops.hlist.Diff
import shapeless.ops.nat.LTEq

import scala.collection.{GenTraversableLike, GenTraversableOnce}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import shapeless.test.illTyped

import scalaz.{Functor, Monoid}

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

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/3
    */

  trait Apart[F]{
    type T
    type W[X]

    def apply(f: F): W[T]
  }

  object Apart_1 {
    def apply[F](implicit apart: Apart[F]) = apart

    implicit def mk[F[_], A] = new Apart[F[A]]{
      type T = A
      type W[X] = F[X]

      def apply(f: F[A]): W[T] = f
    }
  }

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/3/1
    */

    trait Demo[F[_]]{
      type W[X] = F[X]
      type Ignore[X] = F[Int]
      type Identity[X] = X
      type Const[X] = Int
    }

    /**
      * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/3/2
      */
    object Apart_2 {
      def apply[F](implicit apart: Apart[F]) = apart

      type Aux[FA, A, F[_]] = Apart[FA] { type T = A; type W[X] = F[X] }

      implicit def mk[F[_], A]: Aux[F[A], A, F] = new Apart[F[A]]{
        type T = A
        type W[X] = F[X]

        def apply(f: F[A]): W[T] = f
      }
    }

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/3/3
    */
    import scalaz._

    def mapZero[Thing, F[_], A](thing: Thing)
                               (implicit apart: Apart_2.Aux[Thing, A, F],
                                f: Functor[F],
                                m: Monoid[A]): F[A] =
      f.map(apart(thing))(_ => m.zero)


  /**
    * TAKE AWAY
    * 1. You are not restricted to a single abstract type when using dependent types.
    * 2. Type refinements can be used as a mechanism to capture the depedent types within the same implicit declaration.
    * 3. Captured types do not count as a "free" type parameter.
    * 4. Captured types can be used by other type classes to place constraints on types
    */

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/4/1
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/4
    */
  object ApplyEither {

    def apply[T, F, G](t: T, f: F, g: G)
                      (implicit ea: EApply[T, F, G]): ea.Out = ea(t, f, g)
  }

  trait EApply[T, F, G]{
    type Out

    def apply(t: T, f: F, g: G): Out
  }

  /**
    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/4/2
    */

  trait LowPriorityEApply {
    implicit def gapply[T, R, F] = new EApply[T, F, T => R] {
      type Out = R
      def apply(t: T, f: F, g: T => R) = g(t)
    }
  }

  object EApply extends LowPriorityEApply {

    def apply[T, F, G](implicit ea: EApply[T, F, G]) = ea

    implicit def fapply[T, R, G] = new EApply[T, T => R, G] {
      type Out = R
      def apply(t: T, f: T => R, g: G) = f(t)
    }
  }

  val out = ApplyEither(1, {x: Int => 42}, {x: Double => "no"})
  assert(out == 42)

  val out2 = ApplyEither(2.0, {x: Int => 42}, {x: Double => "no"})
  assert(out2 == "no")

//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/4/3
//    */
//
//  /**
//    * Pattern matching on types only works if the types are mutually exclusive
//    * Use implicit resolution order to avoid implicit ambiguity.
//    * Don't forget to use the dependent type to help the compiler avoid "Any" or "AnyRef."
//    */
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/5
//    */
//
//  case class Foo[V](value: V)
//
//  def zero[T, A](t: T)(implicit inner: Inner.Aux[T, A],
//                       m: Monoid[A]): inner.T = m.zero
//
//  zero(Foo(Foo(1))) //won't compile!
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/5/2
//    */
//  trait Unwrap[F]{
//    type Inner
//  }
//
//  object Unwrap extends LowPriorityUnwrap{
//    def apply[F](implicit unwrap: Unwrap[F]) = unwrap
//
//    type Aux[T, A] = Unwrap[T] { type Inner = A }
//
//    implicit def nested[F[_], G](implicit unwrap: Unwrap[G]) =
//      new Unwrap[F[G]]{
//        type Inner = unwrap.Inner
//      }
//  }
//  trait LowPriorityUnwrap{
//    implicit def bottom[F[_], A] =
//      new Unwrap[F[A]]{
//        type Inner = A
//      }
//  }
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/5/3
//    */
//
//  def zero[T, A](t: T)(implicit unwrap: Unwrap.Aux[T, A],
//                       m: Monoid[A]): unwrap.Inner = m.zero
//
//  val out = zero(Foo(Foo(1)))
//  assert(out == 0)
//
//  /**
//    * Dependent types can be used to pass type information around when solving for types.
//    * Implicitly generated traits can be defined in terms of themselves, albeit with different type parameters.
//    * Loops require a type structure that can be recursed over.
//    */
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/6
//    */
//
//  def annoy[A](that: Future[List[Set[Int]]],
//               f: Int => A): Future[List[Set[A]]] =
//    that map{
//      _ map{
//        _ map f
//      }
//    }
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/6/1
//    */
//
//  object MapIt{
//    def apply[A, B, C](in: A, f: B => C)
//                      (implicit mapper: Mapper[A, B, C]): mapper.Out = mapper(in, f)
//  }
//  trait Mapper[A, B, C]{
//    type Out
//
//    def apply(a: A, f: B => C): Out
//  }
//
//  /**
//    * Slide: http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/6/2
//    */
//
//  object Mapper{
//    def apply[A, B, C](implicit mapper: Mapper[A, B, C]) = mapper
//
//    implicit def recur[F[_], A, B, C](implicit nested: Mapper[A, B, C],
//                                      f: Functor[F]) =
//      new Mapper[F[A], B, C] {
//        type Out = F[nested.Out]
//        def apply(fa: F[A], g: B => C): Out = f.map(fa)(nested(_, g))
//      }
//    implicit def base[F[_], A, B >: A, C](implicit f: Functor[F]) =
//      new Mapper[F[A], B, C]{
//        type Out = F[C]
//        def apply(fa: F[A], g: B => C) = f.map(fa)(g)
//      }
//  }
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/6/3
//    */
//
//  val in = List(List(1))
//  val out = MapIt(in, {x: Int => x+1})
//
//  assert(List(List(2)) == out)
//  identity[List[List[Int]]](out)
//
//  /**
//    * http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/6/4
//    */
////  object Mapper{
////    type Aux[A, B, C, Out0] = Mapper[A, B, C]{ type Out = Out0 }
////
////    implicit def recur[F[_], A, B, C]
////    (implicit nested: Mapper[A, B, C],
////     f: Functor[F]): Aux[F[A], B, C, F[nested.Out]] =
////
////    implicit def base[F[_], A, B >: A, C]
////    implicit f: Functor[F]): Aux[F[A], B, C, F[C]] =
////  }
//
//  /**
//    * You can create the mother of all abstractions using dependent types.
//    * Type inference can fail in spectacular ways, especially with dependent types.
//    * Type refinement can help the compiler recapture "lost" type information
//    */
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/7/1
//    */
//
//  import shapeless._
//
//  sealed trait Tree[+T]{
//    type N <: Nat
//  }
//  object Leaf extends Tree[Nothing]{
//    type N = _0
//  }
//  trait Branch[T] extends Tree[T]{
//    def value: T
//    def left: Tree[T]
//    def right: Tree[T]
//  }
//
//  /**
//    * http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/7/3
//    */
//
//  object Branch {
//    type Aux[T, N0 <: Nat] = Branch[T] { type N = N0 }
//
//    def apply[T, D <: Nat](value0: T, left0: Tree[T], right0: Tree[T])
//                          (implicit diff: Diff.Aux[left0.N, right0.N, D],
//                           lte: LTEq[D, _1]): Aux[T, Succ[left0.N]] =
//      new Branch[T] {
//        val value = value0
//        val left = left0
//        val right = right0
//        type N = Succ[left0.N]
//      }
//  }
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/7/4
//    */
//
//  val b1 = Branch(1, Leaf, Leaf)
//  val b2 = Branch(2, Leaf, Leaf)
//  val higher = Branch(3, b1, Leaf)
//  val higher2 = Branch(4, b2, b1)
//
//  illTyped("val nope = Branch(42, higher, Leaf) //won't compile")
//  illTyped("val nadda = Branch(42, Leaf, b1) //won't do it either")
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/7/5
//    */
//  val lower = Branch(1, Leaf, Leaf)
//  illTyped("Branch(42, Leaf, lower)")
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/7/6
//    */
//
//  object Branch{
//    def apply[T](value: T, left0: Tree[T], right0: Tree[T])
//                (implicit tb: TreeBuilder[T, left0.N, right0.N]): Aux[T, tb.Height] =
//      tb(value, left0, right0)
//  }
//  trait TreeBuilder[T, N <: Nat, M <: Nat]{
//    type Height <: Nat
//    def apply(v: T, l: Tree[T], r: Tree[T]): Branch.Aux[T, Height]
//  }
//
//  /**
//    * Slide http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/7/7
//    */
//
//  object TreeBuilder with LowPriorityTreeBuilder{
//    def apply[T, N <: Nat, M <: Nat]
//    (implicit tb: TreeBuilder[T, N, M]): Aux[T, N, M, tb.Height] = tb
//
//    type Aux[T, N <: Nat, M <: Nat, Height0 <: Nat] =
//       TreeBuilder[T, N, M] {type Height = Height0}
//
//    implicit def mkLeft[T, N0 <: Nat, M <: Nat, D <: Nat]
//    (implicit diff: Diff.Aux[N0, M, D],
//     lte: LTEq[D, _1]): Aux[T, N0, M, Succ[N0]] =
//      new TreeBuilder[T, N0, M] { self =>
//        type Height = Succ[N0]
//        def apply(value0: T, left0: Tree[T], right0: Tree[T]) =
//          new Branch[T] {
//            val value = value0
//            val left = left0
//            val right = right0
//            type N = self.Height
//          }
//      }
//  }
//
//  /**
//    * Proofs are only as good as their use can be enforced, i.e. don't let your code work around it.
//    * Multiple valid conditions sometimes require the use of a secondary type class that nests the proof.
//    * Sometimes the type refinement has to be defined by the type that has paradoxically been "lost."
//    */
}
