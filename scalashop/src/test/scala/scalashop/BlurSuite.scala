package scalashop

import org.junit._

import scala.language.postfixOps
import scala.util.Sorting

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  val maxDepth = 3;

  @Test
  def mergeSort(): Unit = {
    // create an array of random 10m random ints
    val r = scala.util.Random
    val xs = (for (i <- 1 to 10) yield r.nextInt(10000)).toArray

    val ys = new Array[Int](xs.length)

    xs.foreach(println)

    sort(xs, ys, 0, xs.length, 0)
    ys.foreach(println)
    List.concat()
  }


  def sort(xs: Array[Int], ys: Array[Int], from: Int, until: Int, depth: Int): Unit = {
    if (depth == maxDepth) {
      Sorting.quickSort(xs)
    } else {
      val mid = (until - from) /2
      parallel(
        sort(xs, ys, from, mid, depth + 1),
        sort(xs, ys, mid, until, depth + 1))

      val flip = (maxDepth -depth) %2 ==0
      val src = if (flip) ys else xs
      val dst = if (flip) xs else ys
      merge(src, dst, from, mid, until)
    }
  }

  def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int): Unit = {
    var i = from
    var j = mid

    var _start = i
    var left = false
    var right = false
    while (i < mid && j < until) {
        if (src(i) < src(i + mid)) {
          if (right) {
            copy(src, dst, _start, j, 0)
            left = false
            right = false
          } else {
            if (!left) {
              _start = i
              left = true
            }

            i = i+1
          }
        } else {
          if (left) {
            copy(src, dst, _start, i, 0)
            left = false
            right = false
          } else {
            if (!right) {
              _start = j
              right = true
            }

            j = j+1
          }
        }
      }
  }

  def copy(src: Array[Int], target: Array[Int], from: Int, until: Int, depth: Int): Unit = {
    if (depth == maxDepth || from == until-1) {
      Array.copy(src, from, target, from, until - from)
    } else {
      val mid = (from +until) /2
      copy(src, target, from, mid, depth +1)
      copy(src, target, mid, from, depth +1)
    }
  }
}
