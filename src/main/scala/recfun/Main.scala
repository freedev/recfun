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
       def checkCoin(coins: List[Int], coin: Int, acc: Int, counter: Int): Int = {
         var c = 0
         if (acc < money) {
           c = checkCoin(coins, coin, acc + coin, counter)
         } else if (acc == money) {
           c = counter + 1
         } else if (acc > money) {
           c = counter
         }
         if (!coins.isEmpty)
           c = checkCoin(coins.tail, coins.head, acc, c)
         c
       }
       checkCoin(coins.tail, coins.head, 0, 0)
    }
  }
