package renderer

import scala.math.max

import utility.Vec._
import utility.Mat44

abstract class Shader {
  def vertex(iFace: Int, nthVer: Int): Vec4
  def fragment(bar: Vec3): Option[Array[Double]]
}

class GourandShader(model: Model) extends Shader {
  var varyingIntensity = Vec3()
  var uniformLightDir = Vec3()
  var uniformM = Mat44()
  var uniformViewport = Mat44()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingIntensity(nthVer) = max(0.0, model.normal(iFace, nthVer).dot(uniformLightDir))
    val glVertex = Vec4.fromVec3(model.vert(iFace, nthVer))
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    val intensity = varyingIntensity.dot(bar)
    Some(Array(255,255*intensity, 255*intensity, 255*intensity))
  }
}

class PhongShader(model: Model) extends Shader {
  var varyingUV = Array.fill(3)(Vec2())
  var varyingNorm = Array.fill(3)(Vec3())
  var uniformM = Mat44()
  var uniformMIT = Mat44()
  var uniformLightDir = Vec3()
  var uniformViewport = Mat44()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingUV(nthVer) = model.textVert(iFace, nthVer)
    varyingNorm(nthVer) = Vec4.projToVec3(uniformMIT * Vec4.fromVec3(model.normal(iFace, nthVer)))
    val glVertex = Vec4.fromVec3(model.vert(iFace, nthVer))
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    var uv = Vec2()
    var n = Vec3()
    for (i <- 0 until 3) {
      uv += varyingUV(i)*bar(i)
      n += varyingNorm(i)*bar(i)
    }
    //val n = (Vec4.projToVec3(uniformMIT * Vec4.fromVec3(model.normalFromMap(uv(0).toInt, uv(1).toInt)))).normalise()
    n = n.normalise()
    val l = (Vec4.projToVec3(uniformM * Vec4.fromVec3(uniformLightDir))).normalise()
    val intensity = max(0.0, n.dot(l))
    val colour = model.diffuse.value(uv.x, uv.y)
    Some(Array(colour(0),colour(1)*intensity, colour(2)*intensity, colour(3)*intensity))
  }
}

class NMShader(model: Model) extends Shader {
  var varyingUV = Array.fill(3)(Vec2())
  var uniformM = Mat44()
  var uniformMIT = Mat44()
  var uniformLightDir = Vec3()
  var uniformViewport = Mat44()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingUV(nthVer) = model.textVert(iFace, nthVer)
    val glVertex = Vec4.fromVec3(model.vert(iFace, nthVer))
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    var uv = Vec2()
    for (i <- 0 until 3) {uv += varyingUV(i)*bar(i)}
    val n = (Vec4.projToVec3(uniformMIT * Vec4.fromVec3(model.normalFromMap(uv.x, uv.y)))).normalise()
    val l = (Vec4.projToVec3(uniformM * Vec4.fromVec3(uniformLightDir))).normalise()
    val intensity = max(0.0, n.dot(l))
    val colour = model.diffuse.value(uv.x, uv.y)
    Some(Array(colour(0),colour(1)*intensity, colour(2)*intensity, colour(3)*intensity))
  }
}