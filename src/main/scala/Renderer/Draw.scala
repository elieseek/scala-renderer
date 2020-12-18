package Renderer

import java.awt.image.BufferedImage
import scala.math.abs

import Display.Image

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
}