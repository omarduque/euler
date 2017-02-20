
import Array._

object Runner {
  def main(args: Array[String]): Unit = {
    val argstuple = args.splitAt(1)
    val methodName = argstuple.productElement(0).asInstanceOf[Array[String]]
    val argsAny = argstuple.productElement(1)
    val euler = new Euler
    val eulerMethod = euler.getClass.getMethod(methodName(0).toString, "".getClass)
    val argsArray= argsAny.asInstanceOf[Array[String]]
    val results = argsArray.map(m => eulerMethod.invoke(euler,m))
    println(results.mkString("\n"))
  }
}
class Euler{
  def euler1(number: String): Integer = {
    val evaluate:Integer = number.toInt
    (0 until evaluate).filter(x => x % 3 == 0 || x % 5 == 0).reduceLeft(_ + _)
  }
  def euler2(number: String): Int = {
    val limit:Int = number.toInt
    fibonacci_ul(limit,null,null).filter(_ % 2 == 0).reduceLeft(_ + _)
  }
  def fibonacci_ul(upper_limit: Int,
                   fibonacci: Any,
                   position: Any): Array[Int] = (upper_limit,fibonacci,position) match {
                      case (0, null, null) => Array(0)
                      case (1, null, null) => Array(0,1)
                      case (_: Int, null, null) if (upper_limit >= 2) => fibonacci_ul(upper_limit,
                                                                                      Array(0,1,2),
                                                                                      2)
                      case (ul: Int, fbnci: Array[Int], pstn: Int) if (fbnci(pstn) < ul) => fibonacci_ul(ul,
                                                                                                         concat(fbnci,
                                                                                                                Array((fbnci(pstn) + fbnci(pstn-1)))),
                                                                                                         pstn+1)
                      case (ul: Int, fbnci: Array[Int], pstn: Int) => fbnci
                      case (_, null, null)=> throw new IllegalArgumentException("expected int and got a upper_limit: "+ upper_limit.getClass)
                    }
}