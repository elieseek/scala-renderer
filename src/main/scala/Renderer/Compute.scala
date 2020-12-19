package renderer

import scala.math.abs

import utility.Vec3Util
import utility.Vec3
import utility.Vec2

object Compute {
  def barycentric(pts: Array[Vec2], p: Array[Int]): Vec3 = {
    val u = Vec3Util.cross(Vec3(pts(2)(0)-pts(0)(0), pts(1)(0)-pts(0)(0), pts(0)(0)-p(0)), Vec3(pts(2)(1)-pts(0)(1), pts(1)(1)-pts(0)(1), pts(0)(1)-p(1)))
    if (abs(u(2)) < 1) {
      Vec3(-1,1,1)
    } else {
      Vec3(1.0 - (u(0)+u(1)).toDouble/u(2).toDouble, u(1).toDouble/u(2).toDouble, u(0).toDouble/u(2))
    }
  } 
}
