package Renderer

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Model(filename: String) {
  var verts = ArrayBuffer[Array[Double]]()
  var faces = ArrayBuffer[Array[Int]]()

  val bufferedSource = Source.fromResource(filename)
  for (line <- bufferedSource.getLines) {
    if (line.startsWith("v ")) {
      val v: Array[Double] = line.drop(2).split(" ").map(x => x.toDouble)
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

  def vert(i: Int): Array[Double] = {
    verts(i)
  }
}