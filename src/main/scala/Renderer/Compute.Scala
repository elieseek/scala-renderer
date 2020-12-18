package Renderer

import scala.math.abs

import Utility.MathUtil

object Compute {
  def barycentric(pts: Array[Array[Int]], p: Array[Int]): Array[Double] = {
    val u = MathUtil.crossProduct(Array(pts(2)(0)-pts(0)(0), pts(1)(0)-pts(0)(0), pts(0)(0)-p(0)), Array(pts(2)(1)-pts(0)(1), pts(1)(1)-pts(0)(1), pts(0)(1)-p(1)))
    if (abs(u(2)) < 1) {
      Array(-1,1,1)
    } else {
      Array(1.0 - (u(0)+u(1)).toDouble/u(2).toDouble, u(1).toDouble/u(2).toDouble, u(0).toDouble/u(2).toDouble)
    }
  } 
}