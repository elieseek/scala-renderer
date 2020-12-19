package display

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import java.nio.Buffer

import utility.MathUtil

object Image {
  def writePixel(image: BufferedImage, x: Int, y: Int, argb: Array[Int]) {
    val u = MathUtil.clamp(x, 0, image.getWidth()-1)
    val v = MathUtil.clamp(image.getHeight()-y, 0, image.getHeight()-1)
    image.setRGB(u, v, (argb(0) << 24) | (argb(1) << 16) | (argb(2) << 8) | argb(3)) // (alpha << 24) | (red << 16 ) | (green<<8) | blue
  }

  def savePNG(image: BufferedImage) {
    val outputFile = new File("image.png")
    ImageIO.write(image, "png", outputFile)
  }
}
