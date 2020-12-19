package renderer

import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import scala.math.floorMod
import scala.util.Random
import scala.math.sqrt

import display.Image
import debug.Timer
import utility.MathUtil
import utility.Vec3
import utility.Vec2
import utility.Vec3Util

object Main extends App {
  val width = 750
  val height = 750
  val image = new BufferedImage(width,height, BufferedImage.TYPE_INT_ARGB)
  val model = new Model("head.obj", "/african_head_diffuse.png")

  val lightDir = Vec3Util.normalise(Vec3(0.0,0.0,-1.0))
  var zBuffer = Array.fill[Double](width*height)(Double.NegativeInfinity)
  for (face <- model.faces) {
    var screenCoords: Array[Vec3] = Array.fill[Vec3](3)(Vec3())
    var worldCoords: Array[Vec3] = Array.fill[Vec3](3)(Vec3())
    var diffuseCoords: Array[Vec3] = Array.fill[Vec3](3)(Vec3())
    for (i <- 0 until 3) {
      val v = model.vert(face(i)(0))
      val vt = model.textVert(face(i)(1))
      screenCoords(i) = Vec3(((v(0)+1.0)*width/2.0), ((v(1)+1.0)*height/2.0), v(2))
      worldCoords(i) = v
      diffuseCoords(i) = vt
    }
    val n = Vec3Util.normalise(Vec3Util.cross(worldCoords(2)-worldCoords(0), worldCoords(1)-worldCoords(0)))
    val intensity = n.dot(lightDir)
    if (intensity > 0) {
      Draw.triangle(screenCoords,diffuseCoords, zBuffer, image, model, intensity)
    }
  }
  
  val viewer = new display.Window(width,height)
  viewer.setImage(image)
}