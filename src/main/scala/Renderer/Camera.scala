package renderer

import utility.Mat44

object Camera {

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