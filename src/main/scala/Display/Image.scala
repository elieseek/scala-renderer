package display

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import java.nio.Buffer

import utility.MathUtil

object Image {
  def writePixel(image: BufferedImage, x: Int, y: Int, rgb: Array[Int]) {
    val u = MathUtil.clamp(x, 0, image.getWidth()-1)
    val v = MathUtil.clamp(image.getHeight()-y, 0, image.getHeight()-1)
    image.setRGB(u, v, ((rgb(0)*65536) + (rgb(1)*256) + rgb(2)) & 0xffffff)
  }

  def savePNG(image: BufferedImage) {
    val outputFile = new File("image.png")
    ImageIO.write(image, "png", outputFile)
  }
}
