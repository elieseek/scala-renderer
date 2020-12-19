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
  val model = new Model("head.obj")
  val rand = new Random(42)
  val lightDir = Vec3Util.normalise(Vec3(0.0,0.0,-1.0))
  for (face <- model.faces) {
    var screenCoords: Array[Vec2] = Array(Vec2(), Vec2(), Vec2())
    val worldCoords: Array[Vec3] = Array(Vec3(), Vec3(), Vec3())
    for (i <- 0 until 3) {
      val v = model.vert(face(i))
      screenCoords(i) = Vec2(((v(0)+1.0)*width/2.0), ((v(1)+1.0)*height/2.0))
      worldCoords(i) = v
    }
    val n = Vec3Util.normalise(Vec3Util.cross(worldCoords(2)-worldCoords(0), worldCoords(1)-worldCoords(0)))
    val intensity = n.dot(lightDir)
    if (intensity > 0) {
      Draw.triangle(screenCoords, image, Array(255, (sqrt(intensity)*255).toInt, (sqrt(intensity)*255).toInt, (sqrt(intensity)*255).toInt))
    }
  }
  
  val viewer = new display.Window(width,height)
  viewer.setImage(image)
}