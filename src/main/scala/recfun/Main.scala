package recfun

import scala.annotation.tailrec

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
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def loop(chars: List[Char], acc: Int): Int = {
        if (chars.isEmpty) {
          acc
        } else {
          val head = chars.head
          if (head == '(') {
            loop(chars.tail, acc + 1)
          } else if (head == ')') {
            if (acc > 0)
              loop(chars.tail, acc - 1)
            else 
              -1
          } else
            loop(chars.tail, acc)
        }
      }
      loop(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        
//      @tailrec
      def countCoins(s: List[Int], rest: Int, counter: Int): Int = {
        s match {
          case Nil => counter
          case xs :: xt => {
            val curRest = rest - s.head
            if (!(curRest < 0)) {
              if (curRest >= xs) {
                if (s.size > 1) {
                  countCoins(s, curRest, countCoins(xt, curRest, counter) )
                } else
                  countCoins(s, curRest, counter)
              } else if (curRest == 0 && s.size == 1) {
                 counter + 1
              } else
                 countCoins(xt, curRest, counter)
            } else
              counter
          }
        }
      }
  
      def getCombinations(prefix: List[Int], s: List[Int], counter : Int): Int = {
        if (s.size > 0) {
          val curComb = prefix :+ s.head
          getCombinations(prefix, s.tail, getCombinations(prefix :+ s.head, s.tail, countCoins(curComb, money, counter)));
        } else
          counter
      }
  
      val s = coins.sorted.reverse
      getCombinations(List(), s, 0)
    }
  }
