package renderer

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import utility.Vec._
import display.Image
import display.Image.FastRGB

class Model(modelName: String, textureName: String) {
  var verts = ArrayBuffer[Vec3]()
  var faces = ArrayBuffer[Array[Array[Int]]]()
  var textVerts = ArrayBuffer[Vec3]()
  var diffuse = new FastRGB(Image.readPNG(textureName))

  val bufferedSource = Source.fromResource(modelName)
  for (line <- bufferedSource.getLines) {
    if (line.startsWith("v ")) {
      val v: Vec3 = Vec3(line.drop(2).split(" ").map(x => x.toDouble))
      verts += v
    } else if (line.startsWith("f ")) {
      var f: Array[Array[Int]] = line.drop(2).split(" ").map(x => x.split("/").map(y => y.toInt-1))
      faces += f
    } else if (line.startsWith(("vt  "))) {
      var vt: Vec3 = Vec3(line.drop(4).split(" ").map(x => x.toDouble))
      textVerts += vt
    }
  }
  //println("v: " + nVerts() + ", f: " + nFaces())
  bufferedSource.close()

  def nVerts() = {
    verts.size
  }

  def nFaces () = {
    faces.size
  }

  def face(idx: Int): Array[Array[Int]] = {
    faces(idx)
  }

  def vert(i: Int): Vec3 = {
    verts(i)
  }

  def textVert(i: Int): Vec3 = {
    textVerts(i)
  }
}