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
}