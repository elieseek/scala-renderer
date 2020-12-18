package Display

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import java.nio.Buffer

object Image {
  def writePixel(image: BufferedImage, x: Int, y: Int, rgb: Array[Int]) {
    image.setRGB(x, image.getHeight()-y-1, ((rgb(0)*65536) + (rgb(1)*256) + rgb(2)) & 0xffffff)
  }

  def savePNG(image: BufferedImage) {
    val outputFile = new File("image.png")
    ImageIO.write(image, "png", outputFile)
  }
}
