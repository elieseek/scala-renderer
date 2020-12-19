package renderer

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import utility.Vec3

class Model(filename: String) {
  var verts = ArrayBuffer[Vec3]()
  var faces = ArrayBuffer[Array[Int]]()
  val bufferedSource = Source.fromResource(filename)
  for (line <- bufferedSource.getLines) {
    if (line.startsWith("v ")) {
      val v: Vec3 = Vec3(line.drop(2).split(" ").map(x => x.toDouble))
      verts += v
    } else if (line.startsWith("f ")) {
      var f: Array[Int] = line.drop(2).split(" ").map(x => x.split("/")(0).toInt - 1)
      faces += f
    }
  }
  println("v: " + nVerts() + ", f: " + nFaces())
  bufferedSource.close()

  def nVerts() = {
    verts.size
  }

  def nFaces () = {
    faces.size
  }

  def face(idx: Int): Array[Int] = {
    faces(idx)
  }

  def vert(i: Int): Vec3 = {
    verts(i)
  }
}