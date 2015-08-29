// 三角数の数列は自然数の和で表わされ, 7番目の三角数は 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28 である. 三角数の最初の10項は:
// 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
// となる.
// 最初の7項について, その約数を列挙すると, 以下のとおり.
//  1: 1
//  3: 1,3
//  6: 1,2,3,6
// 10: 1,2,5,10
// 15: 1,3,5,15
// 21: 1,3,7,21
// 28: 1,2,4,7,14,28
// これから, 7番目の三角数である28は, 5個より多く約数をもつ最初の三角数であることが分かる.
// では, 500個より多く約数をもつ最初の三角数はいくつか.

object Main {
  def main(args: Array[String]) {
    val num: Int = args(0).toInt
    println(question(num, 1))
  }

  def triangularNumberMemo(n:Int): Int = {
    import scala.collection.mutable
    val cache = mutable.Map.empty[Int,Int]

    def triangularNumber(n:Int):Int = {
      n match {
        case n if (n > 2) => triangularNumber(n - 1) + n
        case n if n == 2 => 3
        case n if n == 1 => 1
      }
    }

    if (cache.contains(n)) {
      cache(n)
    } else {
      val v = triangularNumber(n)
      cache += ((n, v))
      v
    }
  }

  def factorsOf(n: Int): (Int, List[Int]) = {
    n match {
      case n if (n > 2)  => (n, Range(1, n + 1).filter((m:Int) => n % m == 0).toList)
      case n if (n == 2) => (n, List(1,2))
      case n if n == 1   => (n, List(1))
    }
  }

  def question(num: Int, n: Int):Int = {
    val x = factorsOf(triangularNumberMemo(n))

//     println(n)
//     println(x._1)
//     println(x._2)

    if (x._2.length >= num) {
      x._1
    } else {
      question(num, n + 1)
    }
  }
}
