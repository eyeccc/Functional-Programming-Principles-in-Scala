package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || r == 0) 1
    else if(r == c) 1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if(chars.isEmpty) true
    else {
      def helperFunc(chars: List[Char]): Int = {
        if(chars.isEmpty) 0
        else if(chars.head == '('){
          val s = 1 + helperFunc((chars.tail));
          if(s > 0) throw new Error("invalid")
          else s
        } else if(chars.head == ')') {
          -1 + helperFunc((chars.tail))
        }else
          helperFunc((chars.tail))
      }

      try {
        val r = helperFunc((chars))
        if(r == 0)true
        else false
      }
      catch {
        case x : Throwable => false
      }
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(coins: List[Int], m: Int, money: Int):Int = {
      if(money == 0) 1
      else if (money < 0) 0
      else if(m <= 0 && money >= 1) 0
      else
          count(coins, m-1, money) + count(coins, m, money - coins(m-1))
    }
    val m = coins.length
    count(coins, m, money)
  }
}
