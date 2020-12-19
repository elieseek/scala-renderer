package utility

import scala.math.sqrt

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

  def normalise(u: Array[Int]): Array[Double] = {
    val sqr = sqrt(dot(u, u))
    u.map(x => x.toDouble/sqr)
  }
  def normalise(u: Array[Double]): Array[Double] = {
    val sqr = sqrt(dot(u, u))
    u.map(x => x/sqr)
  }

  def dot(u: Array[Int], v: Array[Int]): Double = {
    u(0)*v(0) + u(1)*v(1) + u(2)*v(2)
  }
  def dot(u: Array[Double], v: Array[Double]): Double = {
    u(0)*v(0) + u(1)*v(1) + u(2)*v(2)
  }

  def subtract(a: Array[Double], b: Array[Double]): Array[Double] = {
    Array(a(0)-b(0), a(1)-b(1), a(2)-b(2))
  }
  def subtract(a: Array[Int], b: Array[Int]): Array[Int] = {
    Array(a(0)-b(0), a(1)-b(1), a(2)-b(2))
  }
}