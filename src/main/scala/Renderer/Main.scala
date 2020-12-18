package Renderer

import Display.Image
import Debug.Timer
import Utility.MathUtil
import scala.math.floorMod
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

object Main extends App {
  val width = 500
  val height = 500
  val image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
  val model = new Model("head.obj")
  for (face <- model.faces) {
    for (i <- 0 until 3) {
      val v0 = model.vert(face(i))
      val v1 = model.vert(face(MathUtil.posRemainder(i+1,3)))
      val x0 = ((v0(0) + 1.0)*width/2.0).toInt
      val y0 = ((v0(1) + 1.0)*height/2.0).toInt
      val x1 = ((v1(0) + 1.0)*width/2.0).toInt
      val y1 = ((v1(1) + 1.0)*height/2.0).toInt
      Draw.line(x0,y0,x1,y1,image,Array(255,255,255))
    }
  }
  val viewer = new Display.Window(width,height)
  viewer.setImage(image)
}