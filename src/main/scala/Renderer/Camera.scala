package renderer

import utility.Mat44
import utility.Vec._

class Camera(pos: Vec3, dir: Vec3, w: Int, h: Int) {
  var cameraPos = pos
  var viewDir = dir
  var width = w
  var height = h
  var viewport = CameraUtil.viewport(width/8, height/8, width*3/4, height*3/4)
  var projectionMatrix = CameraUtil.projectionMatrix(cameraPos.z)

  def updateCameraPos(pos: Vec3, dir: Vec3) = {
    cameraPos = pos
    viewDir = dir
    projectionMatrix = CameraUtil.projectionMatrix(cameraPos.z)
  }

  def updateViewport(w: Int, h: Int) = {
    width = w
    height = h
    viewport = CameraUtil.viewport(width/8, height/8, width*3/4, height*3/4)
  }
}

object CameraUtil {
  def viewport(x: Double, y: Double, w: Double, h: Double, depth: Double = 255.0) = {
    Mat44(Array(
      Array(w/2.0, 0.0, 0.0, x+w/2.0),
      Array(0.0, h/2.0, 0.0, y+h/2.0),
      Array(0.0, 0.0, depth/2.0, depth/2.0),
      Array(0.0, 0.0, 0.0, 1.0)
    ))
  }
  def projectionMatrix(c: Double) = {
    Mat44(Array(
        Array(1.0,0.0,0.0,0.0),
        Array(0.0,1.0,0.0,0.0),
        Array(0.0,0.0,1.0,0.0),
        Array(0.0,0.0,-1.0/c,1.0)
    ))
  }
}