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
      def loop(chars: List[Char], acc: Int): Int = {
        if (!chars.isEmpty) {
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
        } else
          acc
      }
      loop(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      
//      @tailrec
      def countCoins(s: List[Int], coins: List[Int], rest: Int, counter: Int): Int = {
        if (!s.isEmpty && rest > 0) {
          val curRest = rest - s.head
          if (curRest >= 0) {
            if (curRest >= s.head) {
              if (s.size > 1) {
                  countCoins(s, s.head :: coins, curRest, countCoins(s.tail, s.head :: coins, curRest, counter))
              } else {
                countCoins(s, s.head :: coins, curRest, counter)
              }
            } else if (curRest == 0 && s.size == 1) {
              println(s.head :: coins)
               counter + 1
            } else {
               countCoins(s.tail, s.head :: coins, curRest, counter)
            }
          } else
            counter
        } else
          counter
      }
  
      def getCombinations(s: List[Int]): Int = { 
        def iter(coins: List[List[Int]], counter: Int): Int = {
          coins match {
            case Nil => counter
            case x :: xs => iter(xs, countCoins(x, List(), money, counter))
          }
        }
        iter(innerGetCombinations(List(), s, 0, 0) , 0)
      }
  
      def innerGetCombinations(prefix: List[Int], s: List[Int], counter : Int, depth: Int): List[List[Int]] = {
        if (s.size > 0) {
          val curComb = s.head :: prefix
          val (l1, l2) = (innerGetCombinations(curComb, s.tail, 0, depth + 1), innerGetCombinations(prefix, s.tail, 0, depth + 1))
          (l1, l2) match {
            case (Nil, Nil) => List(curComb) 
            case (x1, Nil) => List(curComb) ::: x1
            case (Nil, x2) => List(curComb) ::: x2
            case (x1, x2) => List(curComb) ::: x1 ::: x2
          }
        } else {
          Nil
        }
      }
  
      val s = coins.sorted
      println("-- START ")
      val ret = getCombinations(s)
      println("-- STOP ")
      ret
    }
  }
