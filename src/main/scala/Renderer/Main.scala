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

import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.Point
import java.awt.RenderingHints.Key

object Main extends App {
  var width = 750
  var height = 750
  var viewportWidth = width
  var viewportHeight = height
  var image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
  val model = new Model("head.obj", "/african_head_diffuse.png")
  val shader = new GourandShader(model)
  
  val cameraPos = Vec3(4, 2, 3)
  val centre = Vec3(0, 0.0, 0)
  val camera = new Camera(cameraPos, centre, width, height)
  val scene = new Scene(Vec3(0, 0, 1))

  val viewer = new display.LiveWindow(width,height)
  viewer.start()
  var fps = 0
  var frames = 0
  var totalTime: Long = 0
  var curTime = System.nanoTime()
  var lastTime = curTime

  viewer.requestFocusInWindow()

  while (true) {
     do {
      lastTime = curTime
      curTime = System.nanoTime()
      totalTime += curTime - lastTime
      if ( totalTime >= 1000000000) {
        totalTime -= 1000000000
        fps = frames
        frames = 0
      }
      frames += 1
      viewportWidth = viewer.getWidth()
      viewportHeight = viewer.getHeight()
      viewer.canvas.setSize(viewportWidth, viewportHeight)
      image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
      Draw.renderFrame(model,shader, scene, camera, image)
      viewer.setFPS(fps)
      viewer.setFrame(image)
    } while (viewer.buffer.contentsLost())
  }
  viewer.dispose()
  System.exit(0)
}