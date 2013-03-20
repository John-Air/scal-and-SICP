package scala.sicp

object Rat {

  case class R(var n: Int, var d: Int) {

    val g = gcd(n, d)

    this.n = n / g
    this.d = d / g

    def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    override def toString(): String = n.toString + "/" + d.toString

  }

  def add(x: R, y: R): R =
    R(x.n * y.d + y.n * x.d, x.d * y.d)

  def sub(x: R, y: R): R =
    R(x.n * y.d - y.n * x.d, x.d * y.d)

  def mul(x: R, y: R): R =
    R(x.n * y.n, x.d * y.d)

  def div(x: R, y: R): R =
    R(x.n * y.d, x.d * y.n)

  def equal(x: R, y: R): Boolean =
    x.n * y.d == x.d * y.n

  def main(args: Array[String]): Unit = {
    System.out.println(R(1, 2))
    System.out.println(add(R(1, 2), R(1, 3)))
    System.out.println(mul(R(1, 2), R(1, 3)))
    System.out.println(add(R(1, 3), R(1, 3)))
  }

}