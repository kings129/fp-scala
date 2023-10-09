package fpinscala.exercises.gettingstarted

object Exercise1 {
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if (n <= 0) current
      else go(n - 1, next, current + next)

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    // Fibonacci sequence 0, 1, 1, 2, 3, 5, 8
    val msg = "The fibonacci of %d is %d"
    println(msg.format(3, fibonacci(3)))
    println(msg.format(5, fibonacci(5)))
    println(msg.format(6, fibonacci(6)))
  }
}

object Exercise2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)
    }

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val a1 = Array(1, 2, 4, 6)
    println(isSorted(a1, (a:Int, b:Int) => a < b))

    val a2 = Array(1, 2, 4, 3)
    println(isSorted(a2, (a: Int, b:Int) => a < b))
  }
}

// exercises 3, 4, 5
object Exercises {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => ( b => f(a, b) )
  }


   def uncurray[A, B, C](f: A => B => C) : (A, B) => C = {
     (a: A, b: B) => f(a)(b)
   }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}


