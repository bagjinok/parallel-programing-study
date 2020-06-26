package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var left = 0
    var right = 0

    for(char <- chars) {
      if (char.equals('(')) left +=1
      else if (char.equals(')') && left > 0) {
        left -= 1
      } else if (char.equals(')')) {
        right += 1
      }
    }

    left == 0 && right == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, left: Int, right: Int): (Int, Int) = {
      var i = idx

      var _left = left
      var _right = right
      while(i < until) {
        val char = chars(i)
        if (char.equals('(')) _left += 1
        else if (char.equals(')') && _left > 0) {
          _left -= 1
        } else if (char.equals(')')) {
          _right += 1
        }

        i+=1
      }

      (_left, _right)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val ((l1, r1), (l2, r2)) = parallel(reduce(from, mid), reduce(mid, until))
        _reduce(l1, r1, l2, r2)
      }
    }

    def _reduce(l1: Int, r1: Int, l2: Int, r2:Int): (Int, Int) = {
      var _left = 0
      var _right = 0
      if (l1 > 0 && l2 > 0) {
        _left = l1 + l2
      } else if (l1 > 0 && r2 > 0) {
        if (l1 == r2) {

        } else if (l1 > r2) {
          _left = l1 - r2
        } else if (l1 < r2) {
          _right = r2 - l1
        }
      } else if (r1 > 0 && l2 > 0) {
        _right = r1
        _left = l2
      } else if (r1 > 0 && r2 > 0) {
        _right = r1 + r2
      }

      (_left, _right)
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
