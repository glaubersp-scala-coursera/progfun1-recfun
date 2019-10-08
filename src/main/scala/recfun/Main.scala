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
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || c == 0 && r >= c || c == r) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = balance_aux(chars, 0)

  @tailrec
  def balance_aux(chars: List[Char], i: Int): Boolean = {
    chars match {
      case Nil => i == 0
      case ::(head, tl) =>
        if (i < 0) false
        else if (head == '(')
          balance_aux(tl, i + 1)
        else if (head == ')')
          balance_aux(tl, i - 1)
        else balance_aux(tl, i)
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else
      coins match {
        case Nil => 0
        case ::(coin, list) =>
          if (coin > money) countChange(money, list)
          else countChange(money - coin, coin :: list) + countChange(money, list)
      }
  }
}
