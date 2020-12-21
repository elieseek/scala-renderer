package renderer

import scala.math.max

import utility.Vec._
import utility.Mat44

abstract class Shader {
  def vertex(iFace: Int, nthVer: Int, scene: Scene, camera: Camera): Vec4
  def fragment(bar: Vec3): Option[Array[Double]]
}

class GourandShader(model: Model) extends Shader {
  var varyingIntensity = Vec3()
  var textCoords = Array(Vec3(), Vec3(), Vec3())
  var viewDir = Vec3()

  def vertex(iFace: Int, nthVer: Int, scene: Scene, camera: Camera): Vec4 = {
    varyingIntensity(nthVer) = max(0.0, model.normal(iFace, nthVer).dot(scene.lightDir))
    textCoords(nthVer) = model.textVert(iFace, nthVer)
    viewDir = camera.viewDir
    val glVertex = Vec4.fromVec3(model.vert(iFace, nthVer))
    camera.viewport * camera.projectionMatrix * camera.modelViewMatrix * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    val intensity = varyingIntensity.dot(bar)
    var textureCoords = Vec3()
    for (i <- 0 until 3) {
      textureCoords += textCoords(i) * bar(i)
    }
    val colour = model.diffuse.value(textureCoords(0), textureCoords(1))
    Some(Array(colour(0), colour(1)*intensity, colour(2)*intensity, colour(3)*intensity))
  
  }
}