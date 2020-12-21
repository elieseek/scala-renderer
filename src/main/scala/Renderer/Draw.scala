package renderer

import java.awt.image.BufferedImage
import java.awt.Color
import scala.math.abs
import scala.math.max
import scala.math.min

import display.Image
import utility.Vec._
import utility.Mat44
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
  def triangle(pts: Array[Vec3], textPts: Array[Vec3], zBuffer: Array[Double], image: BufferedImage, model: Model, intensities: Array[Double]) = {
    var bboxMin = Array(image.getWidth()-1, image.getHeight()-1)
    var bboxMax = Array(0,0)
    var clamp = Array(image.getWidth()-1, image.getHeight()-1)
    for (i <- 0 until 3) {
      for (j <- 0 until 2) {
        bboxMin(j) = max(0, min(bboxMin(j), pts(i)(j).toInt))
        bboxMax(j) = min(clamp(j), max(bboxMax(j), pts(i)(j).toInt))
      }
    }
    
    val diffuse: Image.FastRGB = model.diffuse
    val diffHeight = diffuse.height
    val diffWidth = diffuse.width

    for (x <- bboxMin(0) to bboxMax(0)) {
      for (y <- bboxMin(1) to bboxMax(1)) {
        val bcScreen = Compute.barycentric(pts, Array(x, y, 0))
        if (bcScreen(0)>=0 && bcScreen(1)>=0 && bcScreen(2)>=0 ) {
          val width = image.getWidth()
          var z: Int = 0
          var textureInterp = Vec3()
          var intensity = 0.0
          for (i <- 0 until 3) {
            z += (pts(i)(2)*bcScreen(i)).toInt
            textureInterp += textPts(i) * bcScreen(i)
            intensity += intensities(i) * bcScreen(i)
          }
          val colour = diffuse.value(textureInterp(0), textureInterp(1))
          val zbufferIndex = x+y*width
          if (zBuffer((zbufferIndex).toInt) < z) {
            zBuffer(zbufferIndex) = z
            Image.writePixel(image, x, y, Array((intensity*colour(1)).toInt, (intensity*colour(2)).toInt, (intensity*colour(3)).toInt))
          }
        }
      }
    }
  }

  def renderFrame(model: Model, camera: Camera, image: BufferedImage) = {
    val lightDir = Vec3(0.0,0.0, 1.0).normalise()
    var zBuffer = Array.fill[Double](camera.width*camera.height)(Double.NegativeInfinity)
    val m = camera.viewport * camera.projectionMatrix * camera.modelViewMatrix
    val mInv = m.inverse()
    for (face <- model.faces) {
      var screenCoords: Array[Vec3] = Array.fill[Vec3](3)(Vec3())
      var worldCoords: Array[Vec3] = Array.fill[Vec3](3)(Vec3())
      var diffuseCoords: Array[Vec3] = Array.fill[Vec3](3)(Vec3())
      var intensities: Array[Double] = Array.fill(3)(0.0)
      for (i <- 0 until 3) {
        val v = model.vert(face(i)(0))
        val vt = model.textVert(face(i)(1))
        val vn = model.normVert(face(i)(2))
        screenCoords(i) = Vec4.augmentToVec3(m * Vec4.fromVec3(v))
        worldCoords(i) = v
        diffuseCoords(i) = vt
        //val normal = Vec4.augmentToVec3(mInv * Vec4.fromVec3(vn))
        intensities(i) = MathUtil.clamp(vn.dot(lightDir), 0.0, 1.0)
      }
      val n = Vec3Util.normalise(Vec3Util.cross(worldCoords(2)-worldCoords(0), worldCoords(1)-worldCoords(0)))
      val backFaceCheck = n.dot(camera.viewDir)
      if (backFaceCheck > 0) {
        Draw.triangle(screenCoords,diffuseCoords, zBuffer, image, model, intensities)
      }
    }
  }
}