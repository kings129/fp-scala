package fpinscala.exercises.errorhandling

sealed trait Option[+A] {
  // Apply f if the option if not None
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  // Apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B]  = {
    map(f).getOrElse(None)
  }

  // Of course, we can also implement `flatMap` with explicit pattern matching.
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(ob)
  }

  // Again, we can implement `OrElse` with explicit pattern matching.
  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + (( throw new Exception("Fail!")): Int)
    }
    catch { case e : Exception => 43}
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = (x => x map f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))


  // test - main

  def main(args: Array[String]): Unit = {
    // failingFn(12)
    val a = failingFn2(12)
    println(a)

    val b = mean(Seq(1.0, 2.0, 3.0, 4.0))
    println("mean: ", b)

    val v = variance(Seq(1.0, 2.0, 3.0, 4.0))
    println("variance: ", v)
    println("variance: ", variance(Seq()))

    val absO: Option[Double] => Option[Double] = lift(math.abs)
    println(absO(None))
    println(absO(Some(-0.2)))
  }
}
