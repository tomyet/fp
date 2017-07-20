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
      if (r == 0 || r == 1 || c == 0 || c == r)
        return 1
      else
        return pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def runBalance(chars: List[Char], bracketScore: Int): Boolean = {
        if (chars.isEmpty)
          return true
        else if (chars.head == '(')
          return runBalance(chars.tail, bracketScore + 1)
        else if (chars.head == ')')
          if (bracketScore == 0)
            return false
          else
            return runBalance(chars.tail, bracketScore - 1)
        else
          return runBalance(chars.tail, bracketScore)
      }

      runBalance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty)
        return 0
      else if (money == 0)
        return 1
      else
        return countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
