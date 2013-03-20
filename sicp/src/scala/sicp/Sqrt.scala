package scala.sicp

object Sqrt {
	
  def abs(x:Double):Double = 
    if(x < 0) -x else x 
      
  def average(x: Double, y: Double): Double = 
    (x + y) / 2

  def square(x: Double): Double = 
    x * x

  def sqrt(x: Double): Double = {

    def isGoodEnough(guess: Double): Boolean =
      abs(square(guess) - x) < 0.001

    def improve(guess: Double): Double =
      average(guess, x / guess)

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess))
        guess
      else
        sqrtIter(improve(guess))

    sqrtIter(1.0)
  }

  def main(args: Array[String]): Unit = {
    System.out.println(sqrt(9))
    System.out.println(sqrt(100 + 37))
  }

}