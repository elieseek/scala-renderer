package renderer

import scala.math.max

import utility.Vec._
import utility.Vec.VecUtil._
import utility.Mat4

abstract class Shader {
  def vertex(iFace: Int, nthVer: Int): Vec4
  def fragment(bar: Vec3): Option[Array[Double]]
}

class GouraudShader(model: Model) extends Shader {
  var varyingIntensity = Vec3()
  var uniformLightDir = Vec3()
  var uniformM = Mat4()
  var uniformViewport = Mat4()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingIntensity(nthVer) = max(0.0, dot(model.normal(iFace, nthVer), uniformLightDir))
    val glVertex = embed(model.vert(iFace, nthVer), 1.0)
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    val intensity = dot(varyingIntensity, bar)
    Some(Array(255,255*intensity, 255*intensity, 255*intensity))
  }
}

class PhongShader(model: Model) extends Shader {
  var varyingUV = Array.ofDim[Vec2](3)
  var varyingNorm = Array.ofDim[Vec3](3)
  var uniformM = Mat4()
  var uniformMIT = Mat4()
  var uniformLightDir = Vec3()
  var uniformViewport = Mat4()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingUV(nthVer) = model.textVert(iFace, nthVer)
    varyingNorm(nthVer) = proj(uniformMIT * embed(model.normal(iFace, nthVer)))
    val glVertex = embed(model.vert(iFace, nthVer))
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
    n = normalise(n)
    val l = normalise(proj(uniformM * embed(uniformLightDir)))
    val intensity = max(0.0, dot(n, l))
    val colour = model.diffuse.value(uv.x, uv.y)
    Some(Array(colour(0),colour(1)*intensity, colour(2)*intensity, colour(3)*intensity))
  }
}

class NMShader(model: Model) extends Shader {
  var varyingUV = Array.fill(3)(Vec2())
  var uniformM = Mat4()
  var uniformMIT = Mat4()
  var uniformLightDir = Vec3()
  var uniformViewport = Mat4()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingUV(nthVer) = model.textVert(iFace, nthVer)
    val glVertex = embed(model.vert(iFace, nthVer))
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    var uv = Vec2()
    for (i <- 0 until 3) {uv += varyingUV(i)*bar(i)}
    val n = normalise(proj(uniformMIT * embed(model.normalFromMap(uv.x, uv.y))))
    val l = normalise(proj(uniformM * embed(uniformLightDir)))
    val intensity = max(0.0, dot(n, l))
    val colour = model.diffuse.value(uv.x, uv.y)
    Some(Array(colour(0),colour(1)*intensity, colour(2)*intensity, colour(3)*intensity))
  }
}