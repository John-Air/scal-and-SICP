package scala.sicp

object BenTest {
  
  def p (): Int = p

  def test1(x: Int, y: Int):Int =
      if (x == 0) 0 else y
  
  def test2(x: =>Int, y: =>Int):Int =
      if (x == 0) 0 else y

  def main(args: Array[String]): Unit = {

    System.out.println(test2(0, p()))
  }
}