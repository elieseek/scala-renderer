package renderer

import scala.math.abs

import utility.Vec._
import utility.Vec.VecUtil._

object Compute {
  def barycentric(pts: Array[Vec4], p: Array[Int]): Vec3 = {
    var s = Array(Vec3(), Vec3(), Vec3())
    for (i <- 2 to 0 by -1) {
      s(i) = Vec3(pts(2)(i) - pts(0)(i), pts(1)(i) - pts(0)(i), pts(0)(i) - p(i))
    }
    
    val u = cross(s(0), s(1))
    if (abs(u(2)) > 1e-2) {
      Vec3(1.0 - (u(0)+u(1))/u(2), u(1)/u(2), u(0)/u(2))
    } else {
      Vec3(-1.0, 1.0, 1.0)
    }
  } 
}
