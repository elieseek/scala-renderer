package renderer

import java.awt.image.BufferedImage
import java.awt.Color
import scala.math.abs
import scala.math.max
import scala.math.min

import display.Image
import utility.Vec3
import utility.Vec2
import utility.Mat44


object Draw {
  def line(x0: Int, y0: Int, x1: Int, y1: Int, image: BufferedImage, colour: Array[Int]) {
    var steep = false
      var u0 = x0
      var v0 = y0
      var u1 = x1
      var v1 = y1
    if (abs(x0-x1) < abs(y0-y1)) { // transpose line if its steep to prevent gaps
      u0 = y0
      v0 = x0
      u1 = y1
      v1 = x1
      steep = true
    }
    if (u0 > u1) {
      val tmpU = u0
      u0 = u1
      u1 = tmpU
      val tmpV = v0
      v0 = v1
      v1 = tmpV
    }
    val dx = u1-u0
    val dy = v1-v0
    val derror = abs(dy) * 2
    var error = 0
    var y = v0

    for (x <- u0 to u1) {
      if (steep) {
        Image.writePixel(image, y, x, colour) // re-transpose to get original line
      } else {
        Image.writePixel(image, x, y, colour)
      }
      error += derror
      if (error > dx) {
        val correction: Int = if (v1>v0) 1 else -1
        y += correction
        error -= dx*2
      }
    }
  }
  def triangle(pts: Array[Vec3], textPts: Array[Vec3], zBuffer: Array[Double], image: BufferedImage, model: Model, intensity: Double) = {
    var bboxMin = Array(image.getWidth()-1, image.getHeight()-1)
    var bboxMax = Array(0,0)
    var clamp = Array(image.getWidth()-1, image.getHeight()-1)
    for (i <- 0 until 3) {
      for (j <- 0 until 2) {
        bboxMin(j) = max(0, min(bboxMin(j), pts(i)(j).toInt))
        bboxMax(j) = min(clamp(j), max(bboxMax(j), pts(i)(j).toInt))
      }
    }
    
    for (x <- bboxMin(0) to bboxMax(0)) {
      for (y <- bboxMin(1) to bboxMax(1)) {
        val bcScreen = Compute.barycentric(pts, Array(x, y, 0))
        if (bcScreen(0)>=0 && bcScreen(1)>=0 && bcScreen(2)>=0 ) {
          val width = image.getWidth()
          var z: Int = 0
          var textureInterp = Vec3()
          for (i <- 0 until 3) {
            z += (pts(i)(2)*bcScreen(i)).toInt
            textureInterp += textPts(i) * bcScreen(i)
          }

          val diffuse: Image.FastRGB = model.diffuse
          val diffHeight = diffuse.height
          val diffWidth = diffuse.width
          val colour = diffuse.value(textureInterp(0), textureInterp(1))
          if (zBuffer((x+y*width).toInt) < z) {
            zBuffer(x+y*width) = z
            Image.writePixel(image, x, y, Array(colour(0), (intensity*colour(1)).toInt, (intensity*colour(2)).toInt, (intensity*colour(3)).toInt))
          }
        }
      }
    }
  }
}