package display

import java.awt.image.BufferedImage
import java.awt.image.DataBufferByte
import javax.imageio.ImageIO
import java.io.File
import java.nio.Buffer

import utility.MathUtil
import utility.Vec._

object Image {
  def writePixel(image: BufferedImage, x: Int, y: Int, rgb: Array[Int]) {
    val u = MathUtil.clamp(x, 0, image.getWidth()-1)
    val v = MathUtil.clamp(image.getHeight()-y, 0, image.getHeight()-1)
    image.setRGB(u, v, (rgb(0) << 16) | (rgb(1) << 8) | rgb(2)) // (alpha << 24) | (red << 16 ) | (green<<8) | blue
  }

  def savePNG(image: BufferedImage) {
    val outputFile = new File("image.png")
    ImageIO.write(image, "png", outputFile)
  }

  def readPNG(filename: String) = {
    val file = getClass.getResource(filename)
    ImageIO.read(file)
  }

  // faster than BufferedImage .getRGB method, returns Array(A,R,G,B)
  class FastRGB(image: BufferedImage) {
    val pixels = image.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
    val width = image.getWidth()
    val height = image.getHeight()
    val hasAlphaChannel = image.getAlphaRaster() != null
    val pixelLength = if (hasAlphaChannel) 4 else 3
    
    def getARGB(x: Int, y: Int): Array[Int] = {
      var pos = (y * pixelLength * width) + (x * pixelLength)
      var argb = Array.fill[Int](4)(255)
      if (hasAlphaChannel) {
        argb(0) = ((pixels(pos) & 0xff).toInt) // Alpha
        pos += 1
      }
      argb(3) = (pixels(pos) & 0xff).toInt // Blue
      pos += 1
      argb(2) = (pixels(pos) & 0xff).toInt // Green
      pos += 1
      argb(1) = (pixels(pos) & 0xff).toInt // Red
      pos += 1
      argb
    }

    def getRGBasVec3(x: Int, y: Int): Vec3 = {
      var pos = (y * pixelLength * width) + (x * pixelLength)
      var rgb = Vec3()
      if (hasAlphaChannel) {
        pos += 1
      }
      rgb(2) = (pixels(pos) & 0xff).toInt // Blue
      pos += 1
      rgb(1) = (pixels(pos) & 0xff).toInt // Green
      pos += 1
      rgb(0) = (pixels(pos) & 0xff).toInt // Red
      pos += 1
      rgb
    }

    def value(u: Double, v: Double) = {
      val cu = MathUtil.clamp(u, 0.0, 1.0)
      val cv = 1.0 - MathUtil.clamp(v, 0.0, 1.0) //flip V image coordinates

      var i = (cu * width).toInt
      var j = (cv * height).toInt

      if (i >= width) i = width - 1
      if (j >= height) j = height - 1

      getARGB(i, j)
    }

    def valueAsVec(u: Double, v: Double) = {
      val cu = MathUtil.clamp(u, 0.0, 1.0)
      val cv = 1.0 - MathUtil.clamp(v, 0.0, 1.0) //flip V image coordinates

      var i = (cu * width).toInt
      var j = (cv * height).toInt

      if (i >= width) i = width - 1
      if (j >= height) j = height - 1

      getRGBasVec3(i, j)
    }
  }
}
