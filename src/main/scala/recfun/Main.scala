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
    def pascal(c: Int, r: Int): Int = c match {
      case x if x==0||x==r => 1
      case _ => pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countOpenParentheses(count: Int, chars: List[Char]): Boolean = {
        if (chars == Nil) {
          if (count==0) {
            true
          }
          else {
            false
          }
        }
        else{
          if(count<0) {
            false
          }
          else if(chars.head == '(') {
            countOpenParentheses(count+1, chars.tail)
          }
          else if(chars.head == ')') {
            countOpenParentheses(count-1, chars.tail)
          }
          else {
            countOpenParentheses(count, chars.tail)
          }
        }        
      }
      
      countOpenParentheses(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def change(m: Int, coinList: List[Int], count: Int): Int =
      m match {
        case _ if m < 0 => count
        case _ if coinList.isEmpty => {
          m match {
            case 0 => count + 1
            case _ => count
          }
        }
        case _ => change(m, coinList.tail, count) + change(m - coinList.head, coinList, count)
      }
      change(money, coins, 0)
    }
  }
