package renderer

import utility.Mat44
import utility.Vec._
import utility.Vec.VecUtil._

class Camera(pos: Vec3, lookAt: Vec3, w: Int, h: Int) {
  var cameraPos = pos
  var centre = lookAt
  var distance = (centre-cameraPos).length
  var viewDir = normalise(centre-cameraPos)
  var width = w
  var height = h
  var viewport = CameraUtil.viewport(width/8, height/8, width*3/4, height*3/4)
  var projectionMatrix = CameraUtil.projectionMatrix(distance)
  var modelViewMatrix = CameraUtil.modelViewMatrix(cameraPos, centre, Vec3(0, 1, 0))

  def updateCameraPos(pos: Vec3, lookAt: Vec3) = {
    cameraPos = pos
    centre = lookAt
    viewDir = normalise(centre-cameraPos)
    distance = (cameraPos - centre).length
    projectionMatrix = CameraUtil.projectionMatrix(distance)
    modelViewMatrix = CameraUtil.modelViewMatrix(cameraPos, centre, Vec3(0, 1, 0))
  }

  def updateViewport(w: Int, h: Int) = {
    width = w
    height = h
    viewport = CameraUtil.viewport(width/8, height/8, width*3/4, height*3/4)
  }

  def shiftCamera(shift: Vec3) = {
    updateCameraPos(cameraPos + shift, centre)
  }
}

object CameraUtil {
  def viewport(x: Double, y: Double, w: Double, h: Double, depth: Double = 255.0) = {
    var m = Mat44.identity()
    m.setElement(0, 3, x+w/2.0)
    m.setElement(1, 3, y+h/2.0)
    m.setElement(2, 3, depth/2.0)

    m.setElement(0, 0, w/2.0)
    m.setElement(1, 1, h/2.0)
    m.setElement(2, 2, depth/2.0)
    m
  }
  def projectionMatrix(c: Double) = {
    var proj = Mat44.identity()
    proj.setElement(3, 2 , -1.0/c)
    proj
  }
  def modelViewMatrix(eye: Vec3, centre: Vec3, up: Vec3) = {
    val z = normalise(eye - centre)
    val x = cross(up, z)
    val y = cross(z, x)

    var modelView = Mat44.identity()

    for (i <- 0 until 3) {
      modelView.setElement(0, i, x(i))
      modelView.setElement(1, i, y(i))
      modelView.setElement(2, i, z(i))
      modelView.setElement(i, 3, -centre(i))
    }
    modelView
  }
}