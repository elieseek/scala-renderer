package renderer

import java.awt.image.BufferedImage
import java.awt.Color
import scala.math.abs
import scala.math.max
import scala.math.min

import display.Image
import utility.Vec._
import utility.Vec.VecUtil._
import utility.Mat4
import renderer.Camera
import utility.MathUtil

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
  

  def triangle(distortedPts: Array[Vec4], shader: Shader, zBuffer: Array[Double], image: BufferedImage, camera: Camera) = {
    val pts = distortedPts.map(v => v/v(3))
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
        val bc = Compute.barycentric(pts, Array(x, y, 0))
        val z = pts(0)(2)*bc.x + pts(1)(2)*bc.y + pts(2)(2)*bc.z
        val w = pts(0)(3)*bc.x + pts(1)(3)*bc.y + pts(2)(3)*bc.z
        val fragDepth = max(0, min(255, (z/w+0.5).toInt))
        val bufferIdx = x+y*image.getWidth()
        if (bc.x>=0 && bc.y>=0 && bc.z>=0 && zBuffer(bufferIdx) <= fragDepth ) {
          shader.fragment(bc) match {
            case Some(colour: Array[Double]) => 
              zBuffer(bufferIdx) = fragDepth
              Image.writePixel(image, x, y, Array(colour(1).toInt, colour(2).toInt, colour(3).toInt))
            case None => // do nothing
          }
          
        }
      }
    }
  }

  def renderFrame(model: Model, shader: Shader, scene: Scene, camera: Camera, image: BufferedImage) = {
    var zBuffer = Array.fill[Double](camera.width*camera.height)(Double.NegativeInfinity)
    for (f <- 0 until model.nFaces) {
      var screenCoords: Array[Vec4] = Array.ofDim[Vec4](3)
      for (i <- 0 until 3) {
        screenCoords(i) = shader.vertex(f, i)
      }
      val n = cross(model.vert(f, 2)-model.vert(f, 0), model.vert(f, 1)-model.vert(f, 0))
      if (dot(n, camera.viewDir) > 1e-5) {
        Draw.triangle(screenCoords,shader, zBuffer, image, camera)
      }
    }
  }
}