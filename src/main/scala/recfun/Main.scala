package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
        if (r > 1 && c > 0 && r > c)
          pascal(c - 1, r - 1) + pascal(c, r - 1)
        else
          1
    }                                             //> pascal: (c: Int, r: Int)Int
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], acc: Int): Int = {
        if (!chars.isEmpty) {
          val head = chars.head
          if (head == '(') {
            loop(chars.tail, acc + 1)
          } else if (head == ')') {
            if (acc > 0)
              loop(chars.tail, acc - 1)
            else
              1
          } else
            loop(chars.tail, acc)
        } else {
          0
        }
      }
      loop(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def loop(money: Int, coins: List[Int], curCoins: List[Int], acc : Int): Int = {
         if (coins.isEmpty && curCoins.isEmpty) {
           acc
         } else {
           val nextAcc = if (sum(curCoins, curCoins, 0) == money) acc + 1 else acc
           val nextCurCoins = if (coins.isEmpty) curCoins.tail else curCoins ::: List(coins.head)
           val nextCoins = if (!coins.isEmpty) curCoins.tail else List()
           loop(money, nextCoins, nextCurCoins, nextAcc)
         }
      }
      def sum(coins: List[Int], curCoins: List[Int], acc : Int): Int = {
        if (acc >= money)
          acc
        else if (curCoins.isEmpty)
          sum(coins, coins, acc)
        else
          sum(coins, curCoins.tail, acc + curCoins.head)
      }
      loop(money, coins.tail, List(coins.head), 0)
    }
  }
