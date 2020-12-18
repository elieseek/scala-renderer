package Renderer

import Display.Image
import Debug.Timer
import Utility.MathUtil
import scala.math.floorMod
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import scala.util.Random

object Main extends App {
  val width = 750
  val height = 750
  val image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
  val model = new Model("head.obj")
  val rand = new Random(42)
  for (face <- model.faces) {
    var screenCoords = Array.ofDim[Int](3,2)
    for (i <- 0 until 3) {
      val worldCoords = model.vert(face(i))
      screenCoords(i) = Array(((worldCoords(0)+1.0)*width/2.0).toInt, ((worldCoords(1)+1.0)*height/2.0).toInt)
    }
    Draw.triangle(screenCoords, image, Array(rand.nextInt(256), rand.nextInt(256), rand.nextInt(256)))
  }
  
  val viewer = new Display.Window(width,height)
  viewer.setImage(image)
}