package display

import scala.swing._
import java.awt.image.BufferedImage
import java.awt.Canvas
import javax.swing._

import display.Image
import java.awt.image.BufferStrategy
import java.awt.Graphics

import utility.Vec._

class Window(width: Int, height: Int) extends MainFrame {
  var image = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)

  val view = new Label() {
    preferredSize = new Dimension(width, height)
    icon = new ImageIcon(image)
  }

  val controls = new FlowPanel {
    contents += new Button {
      text = "Save as PNG"
      reactions += {
        case scala.swing.event.ButtonClicked(_) =>
          Image.savePNG(image)
          this.text = "Saved!"
          Timer(1000) {this.text = "Save as PNG" }
      }
    }
    contents += new Button {
      text = "Close"
      reactions += {
        case scala.swing.event.ButtonClicked(_) =>
          System.exit(0)
      }
    }
  }

  val layout = new FlowPanel {
    contents += view
    contents += controls
  }

  def setImage(i: BufferedImage) {
    image = i
    view.icon = new ImageIcon(image)
  }

  title = "Renderer"
  contents = layout
  pack()
  centerOnScreen()
  visible = true
}

private object Timer {
  def apply(interval: Int, repeats: Boolean = false)(op: => Unit) {
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e : java.awt.event.ActionEvent) = op
    }
    val t = new javax.swing.Timer(interval, timeOut)
    t.setRepeats(repeats)
    t.start()
  }
}

class LiveWindow(w: Int, h: Int) extends JFrame {
  val canvas = new Canvas()
  var buffer: BufferStrategy = null
  var fps = 0

  canvas.setIgnoreRepaint(true)
  canvas.setSize(w, h)
  this.setIgnoreRepaint(true)
  this.add(canvas)
  this.pack()
  this.setVisible(true)
  this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  def start() = {
    canvas.createBufferStrategy(3)
    buffer = canvas.getBufferStrategy()
  }

  def setFrame(image: BufferedImage) = {
    val g = buffer.getDrawGraphics()
    g.drawImage(image, 0, 0, this.getWidth(), this.getHeight() ,null)
    g.setColor(new Color(255, 255, 255))
    g.drawString("FPS: " + fps, 10, 20)
    g.dispose()
    buffer.show()
  }

  def setFPS(x: Int) = {
    fps = x
  }
}