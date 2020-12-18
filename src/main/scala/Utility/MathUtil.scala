package Utility

object MathUtil {
  def clamp[T <% Ordered[T]](i: T, min: T, max: T): T = {
    if (i > max) {
      max
    } else if (i < min) {
      min
    } else {
      i
    }
  }

  def posRemainder(x: Int, base: Int) = {
    val r = x%base
    if (r < 0) {
      r+base
    } else {
      r
    }
  }

  def crossProduct(v: Array[Double], u: Array[Double]): Array[Double] = {
    Array(
      v(1) * u(2) - v(2) * u(1),
      v(2) * u(0) - v(0) * u(2),
      v(0) * u(1) - v(1) * u(0)
    )
  }
  
  def crossProduct(v: Array[Int], u: Array[Int]): Array[Int] = {
    Array(
      v(1) * u(2) - v(2) * u(1),
      v(2) * u(0) - v(0) * u(2),
      v(0) * u(1) - v(1) * u(0)
    )
  }
}