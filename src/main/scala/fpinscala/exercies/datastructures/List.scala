package fpinscala.exercies.datastructures

import scala.annotation.tailrec

trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l : List[Int]): Int =
    foldRight(l, 0)(_ + _)

  def product2(l: List[Double]): Double =
    foldRight(l, 1.0)(_ * _)


  val result = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A) : List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n<=0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]) : List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc,x) => Cons(x, acc))

  // Implement foldRight vis foldLeft using tail recursively to avoid stack overflow
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc,x)=>f(x,acc))

  def appendByFoldRight[A](l: List[A], x: A): List[A] =
    foldRight(l, List[A](x))(Cons(_, _))

  def append[A](l: List[A], r: List[A]) : List[A] =
    foldRight(l,  r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // Test exercise implementation / result
  def main(args: Array[String]): Unit = {
    // 3.1
    println(result)

    // 3.2
    println(tail(List(1,2,3)))
    println(tail(List(1)))
    // println(tail(Nil))

    // 3.3
    println(setHead(List(1,2,3), 100))

    val l = List(1,2,3,4,5)
    // 3.4
    println(drop(l, 3))

    // 3.5
    println(dropWhile(l, (x: Int) => (x <= 2)))

    // 3.6
    println(init(l))

    // 3.9
    println(length(l))

    // 3.11
    println(sum3(l))
    val l2 = List(2.0, 3.0, 4.0)
    println(product3(l2))

    // 3.12
    println(reverse(l))
    // test List[A]() used in reverse
    println(List[Int]())

    // 3.13
    println(foldRightViaFoldLeft(l, 0)(_ + _))

    // 3.14
    println(appendByFoldRight(l, 6))

    // 3.15
    val a = concat(List(List(1,2), List(3,4,5), List(6,7)))
    println(a)
  }
}
