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
    def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r -1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def parenCounter(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) {
          count == 0
        } else {
          val s = chars.head
          val n =
            if (s == '(') count + 1
            else if (s == ')') count - 1
            else count
          if (n >= 0) parenCounter(chars.tail, n)
          else false
        }
      }

      parenCounter(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(m: Int, c: List[Int]): Int = {
        if (c.isEmpty) 0
        else if (m - c.head == 0) 1
        else if (m - c.head < 0) 0
        else countChange(m - c.head, c) + countChange(m, c.tail)
      }
      count(money, coins.sorted)
    }
  }
