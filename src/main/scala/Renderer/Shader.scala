package renderer

import scala.math.max

import utility.Vec._
import utility.Vec.VecUtil._
import utility.Mat4
import utility.Mat3
import utility.Vec

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
    val glVertex = Vec4(model.vert(iFace, nthVer), 1.0)
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    val intensity = dot(varyingIntensity, bar)
    Some(Array(255,255*intensity, 255*intensity, 255*intensity))
  }
}

class PhongShader(model: Model) extends Shader {
  var varyingUV = Array.ofDim[Vec2](3)
  var varyingNorm = Mat3()
  var uniformM = Mat4()
  var uniformMIT = Mat4()
  var uniformLightDir = Vec3()
  var uniformViewport = Mat4()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingUV(nthVer) = model.textVert(iFace, nthVer)
    varyingNorm(nthVer) = Vec3(uniformMIT * Vec4(model.normal(iFace, nthVer), 1.0))
    val glVertex = Vec4(model.vert(iFace, nthVer), 1.0)
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    var uv = Vec2()
    var n = varyingNorm * bar
    for (i <- 0 until 3) {
      uv += varyingUV(i)*bar(i)
    }
    n = normalise(n)
    val intensity = max(0.0, dot(n, uniformLightDir))
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
    val glVertex = Vec4(model.vert(iFace, nthVer), 1.0)
    uniformViewport * uniformM * glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    var uv = Vec2()
    for (i <- 0 until 3) {uv += varyingUV(i)*bar(i)}
    val n = normalise(Vec3(uniformMIT * Vec4(model.normalFromMap(uv.x, uv.y), 1.0)))
    val l = normalise(Vec3(uniformM * Vec4(uniformLightDir, 1.0)))
    val intensity = max(0.0, dot(n, l))
    val colour = model.diffuse.value(uv.x, uv.y)
    Some(Array(colour(0),colour(1)*intensity, colour(2)*intensity, colour(3)*intensity))
  }
}

// https://learnopengl.com/Advanced-Lighting/Normal-Mapping
class TSMShader(model: Model) extends Shader {
  var varyingUV = Mat3() // triangle uv coordiantes
  var varyingNorm = Mat3()
  var ndcTri = Mat3() // triangle in noramlised device coordinates
  var uniformM = Mat4()
  var uniformMIT = Mat4()
  var uniformLightDir = Vec3()
  var uniformViewport = Mat4()

  def vertex(iFace: Int, nthVer: Int): Vec4 = {
    varyingUV(nthVer) = Vec3(model.textVert(iFace, nthVer), 0.0)
    varyingNorm(nthVer) = Vec3(uniformMIT * Vec4(model.normal(iFace, nthVer), 0.0))
    val glVertex = uniformViewport * uniformM * Vec4(model.vert(iFace, nthVer), 1.0)
    ndcTri(nthVer) = Vec3(glVertex/glVertex(3))
    glVertex
  }

  def fragment(bar: Vec3): Option[Array[Double]] = {
    var uv = varyingUV*bar
    var bn = normalise(varyingNorm * bar)

    val edge1 = ndcTri.col(1) - ndcTri.col(0)
    val edge2 = ndcTri.col(2) - ndcTri.col(0)
    val deltaUV1 = varyingUV.col(1) - varyingUV.col(0)
    val deltaUV2 = varyingUV.col(2) - varyingUV.col(0)

    val f = 1.0 / (deltaUV1.x*deltaUV2.y - deltaUV2.x*deltaUV1.y)

    val tangent = Vec3()
    for (i <- 0 until 3) {tangent(i) = f * (deltaUV2.y * edge1(i) - deltaUV1.y * edge2(i))}
    val bitangent = Vec3()
    for (i <- 0 until 3) {bitangent(i) = f * (-deltaUV2.x * edge1(i) + deltaUV1.x * edge2(i))}
    
    val t = normalise(Vec3(uniformM * Vec4(tangent, 0.0)))
    val b = normalise(Vec3(uniformM * Vec4(bitangent, 0.0)))
    val n = normalise(Vec3(uniformM * Vec4(bn, 0.0)))
    val tbn = Mat3(t, b, n)
    
    val normal = normalise(tbn*model.normalFromMap(uv.x, uv.y))
    val intensity = max(0.0, dot(normal, uniformLightDir))

    val colour = model.diffuse.value(uv.x, uv.y)
    Some(Array(colour(0),colour(1)*intensity, colour(2)*intensity, colour(3)*intensity))
  }
}