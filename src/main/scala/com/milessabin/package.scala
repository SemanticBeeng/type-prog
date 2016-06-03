package com


/**
  *
  */
package object milessabin {

  import shapeless.test.illTyped

  /**
    * source: https://gist.github.com/milessabin/89c9b47a91017973a35f
    */
  // Compile with -optimize to eliminate boxing/unboxing in specialized
  // tag methods.

  class User
  class Checkin

  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U] // Thanks to @retronym for suggesting this type alias

  class Tagger[U] {
    def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
  }
  def tag[U] = new Tagger[U]

  // Manual specialization needed here ... specializing apply above doesn't help
  def tag[U](i : Int) : Int @@ U = i.asInstanceOf[Int @@ U]
  def tag[U](l : Long) : Long @@ U = l.asInstanceOf[Long @@ U]
  def tag[U](d : Double) : Double @@ U = d.asInstanceOf[Double @@ U]

  def fetch[A](id: Int @@ A): A = null.asInstanceOf[A]

  def main(args: Array[String]): Unit = {
    val id = tag[Checkin](10)

    fetch[Checkin](id) // Compiles
    illTyped("fetch[User](id)")    // Does not compile

    val ids = tag[User](1) :: tag[User](2) :: tag[User](3) :: Nil

    val users : List[(Int @@ User)] = ids       // Compiles
    illTyped("val checkins : List[Int @@ Checkin] = ids")   // Does not compile
  }
}
