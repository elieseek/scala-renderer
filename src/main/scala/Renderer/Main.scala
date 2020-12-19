package renderer

import display.Image
import debug.Timer
import utility.MathUtil
import scala.math.floorMod
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import scala.util.Random
import scala.math.sqrt

object Main extends App {
  val width = 750
  val height = 750
  val image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
  val model = new Model("head.obj")
  val rand = new Random(42)
  val lightDir = MathUtil.normalise(Array(0.0,0.0,-1.0))
  for (face <- model.faces) {
    var screenCoords = Array.ofDim[Double](3,2)
    val worldCoords = Array.ofDim[Double](3,2)
    for (i <- 0 until 3) {
      val v = model.vert(face(i))
      screenCoords(i) = Array(((v(0)+1.0)*width/2.0), ((v(1)+1.0)*height/2.0))
      worldCoords(i) = v
    }
    val n = MathUtil.normalise(MathUtil.crossProduct(MathUtil.subtract(worldCoords(2),worldCoords(0)), MathUtil.subtract(worldCoords(1),worldCoords(0))))
    val intensity = MathUtil.dot(n, lightDir)
    if (intensity > 0) {
      Draw.triangle(screenCoords, image, Array((sqrt(intensity)*255).toInt, (sqrt(intensity)*255).toInt, (sqrt(intensity)*255).toInt))
    }
  }
  
  val viewer = new display.Window(width,height)
  viewer.setImage(image)
}