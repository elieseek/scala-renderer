package utility

import scala.math.sqrt

import utility.MathUtil.clamp

object Vec {

  class Vec3 {
    var x = 0.0
    var y = 0.0
    var z = 0.0

    def +(that: Vec3) = Vec3(this.x + that.x, this.y + that.y, this.z + that.z)

    def -(that: Vec3) = Vec3(this.x - that.x, this.y - that.y, this.z - that.z)

    def *(t: Double) = Vec3(this.x * t, this.y * t, this.z * t)

    def *(that: Vec3) = Vec3(this.x * that.x, this.y * that.y, this.z * that.z)

    def /(t: Double) = Vec3(this.x / t, this.y / t, this.z / t)
    
    def apply(i: Int) = i match {
      case 0 => this.x
      case 1 => this.y
      case 2 => this.z
    }

    def update(i: Int, j: Double) = i match {
      case 0 => this.x = j
      case 1 => this.y = j
      case 2 => this.z = j
    }

    def length() = sqrt(lengthSquared())

    def lengthSquared(): Double = x*x + y*y + z*z
  }

  object Vec3{
    def apply(a: Array[Double]): Vec3 = {
      var v = new Vec3()
      v.x = a(0)
      v.y = a(1)
      v.z = a(2)
      v
    }
    def apply(x: Double, y: Double, z: Double): Vec3 = {
      var v = new Vec3()
      v.x = x
      v.y = y
      v.z = z
      v
    }
    def apply() = {
      new Vec3
    }

  }

  class Vec2 {
    var x = 0.0
    var y = 0.0

    def +(that: Vec2) = Vec2(this.x + that.x, this.y + that.y)

    def -(that: Vec2) = Vec2(this.x - that.x, this.y - that.y)

    def *(t: Double) = Vec2(this.x * t, this.y * t)

    def *(that: Vec2) = Vec2(this.x * that.x, this.y * that.y)

    def /(t: Double) = Vec2(this.x / t, this.y / t)

    def apply(i: Int) = i match {
      case 0 => this.x
      case 1 => this.y
    }

    def update(i: Int, j: Double) = i match {
      case 0 => this.x = j
      case 1 => this.y = j
    }

    def lengthSquared(): Double = x*x + y*y
    
    def length() = sqrt(lengthSquared())
  }

  object Vec2 {
    def apply(a: Array[Double]): Vec2 = {
      var v = new Vec2()
      v.x = a(0)
      v.y = a(1)
      v
    }
    def apply(x: Double, y: Double): Vec2 = {
      var v = new Vec2()
      v.x = x
      v.y = y
      v
    }
    def apply() = {
      new Vec2
    }

  }

  class Vec4 {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    var w = 0.0
    
    def +(that: Vec4) = Vec4(this.x + that.x, this.y + that.y, this.z + that.z, this.w + that.w)

    def -(that: Vec4) = Vec4(this.x - that.x, this.y - that.y, this.z - that.z,  this.w - that.w)

    def *(t: Double) = Vec4(this.x * t, this.y * t, this.z * t, this.w * t)

    def *(that: Vec4) = Vec4(this.x * that.x, this.y * that.y, this.z * that.z, this.w * that.w)

    def /(t: Double) = Vec4(this.x / t, this.y / t, this.z / t, this.w / t)

    def apply(i: Int) = i match {
      case 0 => this.x
      case 1 => this.y
      case 2 => this.z
      case 3 => this.w
    }

    def update(i: Int, j: Double) = i match {
      case 0 => this.x = j
      case 1 => this.y = j
      case 2 => this.z = j
      case 3 => this.w = j
    }

    def lengthSquared(): Double = x*x + y*y + z*z + w*w

    def length() = sqrt(lengthSquared())
  }

  object Vec4 {
    def apply(a: Array[Double]): Vec4 = {
      var v = new Vec4()
      v.x = a(0)
      v.y = a(1)
      v.z = a(2)
      v.w = a(3)
      v
    }
    def apply(x: Double, y: Double, z: Double, w: Double): Vec4 = {
      var v = new Vec4()
      v.x = x
      v.y = y
      v.z = z
      v.w = w
      v
    }
    def apply() = {
      new Vec4
    }
  }

  object VecUtil {
    def dot(v: Vec2, u: Vec2) = v.x*u.x + v.y*u.y
    def dot(v: Vec3, u: Vec3) = v.x*u.x + v.y*u.y + v.z*u.z
    def dot(v: Vec4, u: Vec4) = v.x*u.x + v.y*u.y + v.z*u.z + v.w*u.w

    def cross(v: Vec3, u: Vec3) = {
      Vec3(
        v.y * u.z - v.z * u.y,
        v.z * u.x - v.x * u.z,
        v.x * u.y - v.y * u.x
      )
    }

    def clampVec3(v: Vec3, min: Double, max: Double) = Vec3(clamp(v.x, min, max), clamp(v.y, min, max), clamp(v.z, min, max))

    def normalise(v: Vec2) = v / v.length
    def normalise(v: Vec3) = v / v.length
    def normalise(v: Vec4) = v / v.length

    def proj(v: Vec4): Vec3 = {
      Vec3(v(0), v(1), v(2))
    }

    def embed(v: Vec3, x: Double = 1.0): Vec4 = {
      Vec4(v(0), v(1), v(2), x)
    }
    
  }
}