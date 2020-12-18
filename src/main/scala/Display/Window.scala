package Display

import scala.swing._
import Display.Image
import java.awt.image.BufferedImage
import javax.swing.ImageIcon

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
        case event.ButtonClicked(_) =>
          Image.savePNG(image)
          this.text = "Saved!"
          Timer(1000) {this.text = "Save as PNG!" }
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