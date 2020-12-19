package renderer

import scala.math.abs

import utility.Vec3Util
import utility.Vec3
import utility.Vec2

object Compute {
  def barycentric(pts: Array[Vec3], p: Array[Int]): Vec3 = {
    var s = Array(Vec3(), Vec3(), Vec3())
    for (i <- 2 to 0 by -1) {
      s(i)(0) = pts(2)(i) - pts(0)(i)
      s(i)(1) = pts(1)(i) - pts(0)(i)
      s(i)(2) = pts(0)(i) - p(i)
    }
    
    val u = Vec3Util.cross(s(0), s(1))
    if (abs(u(2)) > 1e-2) {
      Vec3(1.0 - (u(0)+u(1)).toDouble/u(2).toDouble, u(1).toDouble/u(2).toDouble, u(0).toDouble/u(2))
    } else {
      Vec3(-1.0, 1.0, 1.0)
    }
  } 
}
