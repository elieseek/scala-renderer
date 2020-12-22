package renderer

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import utility.Vec._
import display.Image
import display.Image.FastRGB

class Model(modelName: String, textureName: String, normalName: String) {
  var verts = ArrayBuffer[Vec3]()
  var faces = ArrayBuffer[Array[Array[Int]]]()
  var textVerts = ArrayBuffer[Vec2]() // Texture coordinates are only xy
  var normVerts = ArrayBuffer[Vec3]()
  var diffuse = new FastRGB(Image.readPNG(textureName))
  var normalMap = new FastRGB(Image.readPNG(normalName))
  
  val bufferedSource = Source.fromResource(modelName)
  for (line <- bufferedSource.getLines) {
    if (line.startsWith("v ")) {
      val v: Vec3 = Vec3(line.drop(2).split(" ").map(x => x.toDouble))
      verts += v
    } else if (line.startsWith("f ")) {
      var f: Array[Array[Int]] = line.drop(2).split(" ").map(x => x.split("/").map(y => y.toInt-1))
      faces += f
    } else if (line.startsWith("vt  ")) {
      var vt: Vec2 = Vec2(line.drop(4).split(" ").map(x => x.toDouble).take(2))
      textVerts += vt
    } else if (line.startsWith("vn  ")) {
      var vn: Vec3 = Vec3(line.drop(4).split(" ").map(x => x.toDouble))
      normVerts += vn
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

  def vert(f: Int, v: Int): Vec3 = {
    verts(face(f)(v)(0))
  }

  def textVert(f: Int, v: Int): Vec2 = {
    textVerts(face(f)(v)(1))
  }

  def normal(f: Int, v: Int) = {
    normVerts(face(f)(v)(2))
  }

  def normalFromMap(x: Int, y: Int) = {
    // Transforms (0, 1) -> (-1, 1)
    val normal = normalMap.getRGBasVec3(x, y) / 255.0
    (normal * 2.0 - Vec3(1.0, 1.0, 1.0)).normalise()
  }
}