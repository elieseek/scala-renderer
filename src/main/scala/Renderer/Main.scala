package renderer

import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import scala.math.floorMod
import scala.util.Random
import scala.math.sqrt
import scala.math.sin
import scala.math.cos

import display.Image
import debug.Timer
import utility.MathUtil
import utility.Vec._
import utility.Vec.VecUtil._
import utility.Mat4
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
  val model = new Model("head.obj", "/african_head_diffuse.png", "/african_head_nm.png")
  
  val cameraPos = Vec3(0, 0, 3.0)
  val centre = Vec3(0.0, 0.0, 0.0)
  val camera = new Camera(cameraPos, centre, width, height)
  val scene = new Scene(normalise(Vec3(1, 0, 0)))

  val shader = new NMShader(model)
  shader.uniformM = camera.projectionMatrix * camera.modelViewMatrix
  shader.uniformMIT = (shader.uniformM).inverse().transpose()
  shader.uniformLightDir = scene.lightDir
  shader.uniformViewport = camera.viewport

  val viewer = new display.LiveWindow(width,height)
  viewer.start()
  var fps = 0
  var frames = 0
  var totalTime: Long = 0
  var curTime = System.nanoTime()
  var lastTime = curTime

  viewer.requestFocusInWindow()
  var i: Double = 0.0
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
      shader.uniformLightDir = Vec3(sin(1+i/15.0), 0, cos(1+ i/15.0))
      camera.updateCameraPos(Vec3(3*sin(i/15), 0, 3*cos(i/15)), Vec3(0, 0, 0))
      shader.uniformM = camera.projectionMatrix * camera.modelViewMatrix
      shader.uniformMIT = (shader.uniformM).inverse().transpose()
      shader.uniformViewport = camera.viewport
      viewportWidth = viewer.getWidth()
      viewportHeight = viewer.getHeight()
      viewer.canvas.setSize(viewportWidth, viewportHeight)
      image = new BufferedImage(width,height, BufferedImage.TYPE_INT_RGB)
      Draw.renderFrame(model,shader, scene, camera, image)
      viewer.setFPS(fps)
      viewer.setFrame(image)
      i += 1
    } while (viewer.buffer.contentsLost())
  }
  viewer.dispose()
  System.exit(0)
}