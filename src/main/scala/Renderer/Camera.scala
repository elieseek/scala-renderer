package renderer

import utility.Mat44
import utility.Vec._

class Camera(pos: Vec3, lookAt: Vec3, w: Int, h: Int) {
  var cameraPos = pos
  var centre = lookAt
  var distance = (centre-cameraPos).length()
  var viewDir = (centre-cameraPos).normalise()
  var width = w
  var height = h
  var viewport = CameraUtil.viewport(width/8, height/8, width*3/4, height*3/4)
  var projectionMatrix = CameraUtil.projectionMatrix(distance)
  var modelViewMatrix = CameraUtil.modelViewMatrix(cameraPos, centre, Vec3(0, 1, 0))

  def updateCameraPos(pos: Vec3, lookAt: Vec3) = {
    cameraPos = pos
    centre = lookAt
    viewDir = (centre-cameraPos).normalise()
    distance = (cameraPos - centre).length()
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
    val m = Mat44.identity()
    m.setElement(0, 3, x+w/2.0)
    m.setElement(1, 3, y+h/2.0)
    m.setElement(2, 3, depth/2.0)

    m.setElement(0, 0, w/2.0)
    m.setElement(1, 1, h/2.0)
    m.setElement(2, 2, depth/2.0)
    m
  }
  def projectionMatrix(c: Double) = {
    val proj = Mat44.identity()
    proj.setElement(3, 2 , -1.0/c)
    proj
  }
  def modelViewMatrix(eye: Vec3, centre: Vec3, up: Vec3) = {
    val z = (eye - centre).normalise()
    val x = Vec3Util.cross(up, z).normalise()
    val y = Vec3Util.cross(z, x).normalise()

    var mInv = Mat44.identity()
    var tr = Mat44.identity()

    for (i <- 0 until 3) {
      mInv.setElement(0, i, x(i))
      mInv.setElement(1, i, y(i))
      mInv.setElement(2, i, z(i))
      tr.setElement(i, 3, -centre(i))
    }
    mInv * tr
  }
}