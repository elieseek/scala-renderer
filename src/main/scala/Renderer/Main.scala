package Renderer

import Display.Image
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

object Main extends App {
  val i = new BufferedImage(100,100, BufferedImage.TYPE_INT_RGB)
  Image.writePixel(i,52,41,Array(255,0,0))
  Image.savePNG(i)
  val viewer = new Display.Window(100,100)
  viewer.setImage(i)
}