package renderer

import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import scala.math.floorMod
import scala.util.Random
import scala.math.sqrt
import scala.math.sin

import display.Image
import debug.Timer
import utility.MathUtil
import utility.Vec._
import utility.Mat44
import utility.Vec

object Main extends App {
  var width = 750
  var height = 750
  var viewportWidth = width
  var viewportHeight = height
  var image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
  val model = new Model("head.obj", "/african_head_diffuse.png")

  
  val cameraPos = Vec3(0, 0, 3)
  val viewDir = Vec3Util.normalise(cameraPos*(-1))
  val camera = new Camera(cameraPos, viewDir, width, height)

  
  val viewer = new display.LiveWindow(width,height)
  viewer.start()
  var i = 0
  while (true) {
     do {
      val startTime = System.nanoTime()
      viewportWidth = viewer.getWidth()
      viewportHeight = viewer.getHeight()
      viewer.canvas.setSize(viewportWidth, viewportHeight)
      //camera.updateCameraPos(Vec3(0,0,3 + sin(i)), Vec3Util.normalise(Vec3(0,0,3 + sin(i))*(-1)))
      image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
      i += 1
      Draw.renderFrame(model, camera, image)
      val fps = (1000000000 / (System.nanoTime() - startTime)).toInt
      viewer.setFPS(fps)
      viewer.setFrame(image)
    } while (!viewer.buffer.contentsLost())
  }
}