package reductions

import java.util

import org.scalameter._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    } catch {
      case e: NotImplementedError =>
        println("Not implemented.")
    }

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange extends ParallelCountChangeInterface {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */

//  def countChange(money: Int, coins: List[Int]): Int = {
//    if (money < 0) {
//      0
//    } else if (money == 0) {
//      1
//    } else {
//      if (coins.isEmpty) {
//        0
//      } else {
//        countChange(money - coins.last, coins) + countChange(money, coins.dropRight(1))
//      }
//    }
//  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) {
      0
    } else if (money == 0) {
      1
    } else {
      if (coins.isEmpty) {
        0
      } else {
        checkSmallCoins(money, coins.length -1, coins)
      }
    }
  }

  def checkSmallCoins(leftMoney: Int, currentCoinIndex: Int, coins: List[Int]): Int = {
    if (leftMoney < 0) {
      return 0
    }

    var res = 0
    val coinMoney = coins(currentCoinIndex)

    val max = leftMoney/coinMoney
    if (max == 0) {
      return 0
    }

    var i = max
    while (i >= 0) {
      var current = leftMoney - (i * coinMoney)

      if (current == 0) {
        res += 1
      } else if (current > 0) {
        if (currentCoinIndex >= 1) {
          res += checkSmallCoins(current, currentCoinIndex-1, coins)
        } else {
          return res
        }
      } else {
        return res
      }

      i -= 1
    }

    res
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money < 0) {
      0
    } else if (money == 0) {
      1
    } else {
      if (coins.isEmpty) {
        0
      } else {
        if (threshold(money, coins)) {
          countChange(money, coins)
        } else {
          val (a, b) = parallel(
            parCountChange(money - coins.last, coins, threshold),
            parCountChange(money, coins.dropRight(1), threshold))

          a + b
        }
      }
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (money, list) => money <= ((startingMoney * 2)/3)

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (money, list) => list.size <= ((totalCoins * 2)/3)


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (money, list) => (money * list.size) <= ((startingMoney * allCoins.size) / 2)
  }
}
