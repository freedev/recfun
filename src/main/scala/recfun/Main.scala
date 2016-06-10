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
        var c = acc
        if (!chars.isEmpty) {
          val head = chars.head
          if (head == '(') {
            c = loop(chars.tail, c + 1)
          } else if (head == ')') {
            if (acc > 0)
              c = loop(chars.tail, c - 1)
            else 
              c = -1
          } else
            c = loop(chars.tail, c)
        }
        c
      }
      loop(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        
      def countCoins(s: List[Int], rest: Int, counter: Int): Int = {
        var c = counter
        if (!s.isEmpty) {
          var curRest = rest - s.head
          if (!(curRest < 0)) {
            if (curRest >= s.head) {
              if (s.size > 1) {
                c = countCoins(s.tail, curRest, c)
              }
              c = countCoins(s, curRest, c)
            } else if (curRest == 0 && s.size == 1) {
              c = c + 1
            } else
                c = countCoins(s.tail, curRest, c)
          }
        }
        c
      }
  
      def getCombinations(s: List[Int]): Int = { innerGetCombinations(List(), s, 0) }
  
      def innerGetCombinations(prefix: List[Int], s: List[Int], counter : Int): Int = {
        var c = counter
        if (s.size > 0) {
          val curComb = prefix :+ s.head
          c = countCoins(curComb, money, c)
          c = innerGetCombinations(prefix :+ s.head, s.tail, c);
          c = innerGetCombinations(prefix, s.tail, c);
        }
        c
      }
  
      val s = coins.sorted.reverse
      getCombinations(s)
    }
  }
