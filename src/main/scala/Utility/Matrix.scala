package utility

import Vec._

case class Mat3() {
  var values = Array.ofDim[Double](3,3)

  def +(that: Mat3) = {
    var result = Mat3()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        result.setElement(i, j, that(i, j) + this(i, j)) 
      }
    }
    result
  }

  def *(that: Mat3) = {
    var result = Mat3()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        val sumProd = this(i, 0)*that(0, j) + this(i, 1)*that(1, j) + this(i, 2)*that(2 ,j)
        result.setElement(i, j, sumProd) 
      }
    }
    result
  }

  def *(t: Double) = {
    var result = Mat3()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        result.setElement(i, j, this(i, j)*t) 
      }
    }
    result
  }

  def *(v: Vec3) = {
    var result = Vec3()
    for (i <- 0 until 3) {
      result(i) = this(i, 0)*v(0) + this(i, 1)*v(1) + this(i, 2)*v(2)
    }
    result
  }
  
  def transpose() = {
    var result = Mat3()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        result.setElement(i, j, this(j, i)) 
      }
    }
    result
  } 

  def col(i: Int) = {
    Vec3(this(0, i), this(1, i), this(2, i))
  }

  def apply(i: Int, j: Int) = {
    this.values(i)(j)
  }
  def update(i: Int, j: Int, k: Double) = {
    this.values(i)(j) = k
  }
  def update(i: Int, v: Vec3) = {
    this.values(0)(i) = v(0)
    this.values(1)(i) = v(1)
    this.values(2)(i) = v(2)
  }

  def setElement(i: Int, j: Int, el: Double) = {
    this.values(i)(j) = el
  }
  
  def inverse() = {
    val det = {
      this(0,0) * (this(1,1)*this(2,2) - this(2,1)*this(1,2)) -
      this(0,1) * (this(1,0)*this(2,2) - this(1,2)*this(2,0)) +
      this(0,2) * (this(1,0)*this(2,1) - this(1,1)*this(2,0))
    }
    val invdet = 1.0/det

    var res = Mat3()
    res(0,0) = (this(1,1)*this(2,2)-this(2,1)*this(1,2)) * invdet
    res(0,1) = (this(0,2)*this(2,1)-this(0,1)*this(2,2)) * invdet
    res(0,2) = (this(0,1)*this(1,2)-this(0,2)*this(1,1)) * invdet
    res(1,0) = (this(1,2)*this(2,0)-this(1,0)*this(2,2)) * invdet
    res(1,1) = (this(0,0)*this(2,2)-this(0,2)*this(2,0)) * invdet
    res(1,2) = (this(1,0)*this(0,2)-this(0,0)*this(1,2)) * invdet
    res(2,0) = (this(1,0)*this(2,1)-this(2,0)*this(1,1)) * invdet
    res(2,1) = (this(2,0)*this(0,1)-this(0,0)*this(2,1)) * invdet
    res(2,2) = (this(0,0)*this(1,1)-this(1,0)*this(0,1)) * invdet
    res
  }
}

object Mat3 {
  def apply() = {
    new Mat3
  }
  def apply(array: Array[Array[Double]]) = {
    val mat = new Mat3
    mat.values = array
    mat
  }
  def apply(v1: Vec3, v2: Vec3, v3: Vec3) = {
    val mat = new Mat3
    var i = 0
    for (v <- Array(v1, v2, v3)) {
      for (j <- 0 until 3) {
        mat(j, i) = v(j)
      }
      i += 1
    }
  }
}

case class Mat4() {
  var values = Array.ofDim[Double](4,4)

  def +(that: Mat4) = {
    var result = Mat4()
    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        result.setElement(i, j, that(i, j) + this(i, j)) 
      }
    }
    result
  }

  def *(that: Mat4) = {
    var result = Mat4()
    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        var sumProd = 0.0
         for (k <- 0 until 4) {sumProd += this(i, k)*that(k, j)}
        result.setElement(i, j, sumProd) 
      }
    }
    result
  }

  def *(t: Double) = {
    var result = Mat4()
    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        result.setElement(i, j, this(i, j)*t) 
      }
    }
    result
  }

  def *(v: Vec4) = {
    var result = Vec4()
    for (i <- 0 until 4) {
      result(i) = this(i, 0)*v(0) + this(i, 1)*v(1) + this(i, 2)*v(2) + this(i, 3)*v(3)
    }
    result
  }
  
  def transpose() = {
    var result = Mat4()
    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        result.setElement(i, j, this(j, i)) 
      }
    }
    result
  }

  def col(i: Int) = {
    Vec4(this(0, i), this(1, i), this(2, i), this(3, i))
  }

  def apply(i: Int, j: Int) = {
    this.values(i)(j)
  }
  def update(i: Int, j: Int, k: Double) = {
    this.values(i)(j) = k
  }
  def update(i: Int, v: Vec4) = {
    this.values(0)(i) = v(0)
    this.values(1)(i) = v(1)
    this.values(2)(i) = v(2)
    this.values(3)(i) = v(3)
  }

  def setElement(i: Int, j: Int, el: Double) = {
    this.values(i)(j) = el
  }

  // Ugly but efficient
  def inverse(): Mat4 = {
    var detMat = Mat4()
    detMat.setElement(0, 0, 
    this(1, 1) * this(2, 2) * this(3, 3) - 
    this(1, 1) * this(2, 3) * this(3, 2) - 
    this(2, 1) * this(1, 2) * this(3, 3) + 
    this(2, 1) * this(1, 3) * this(3, 2) + 
    this(3, 1) * this(1, 2) * this (2, 3) - 
    this(3, 1) * this(1, 3) * this(2, 2)
    )
    detMat.setElement(1, 0, 
    -this(1, 0) * this(2, 2) * this(3, 3) + 
    this(1, 0) * this(2, 3) * this(3, 2) + 
    this(2, 0) * this(1, 2) * this(3, 3) - 
    this(2, 0) * this(1, 3) * this(3, 2) - 
    this(3, 0) * this(1, 2) * this (2, 3) + 
    this(3, 0) * this(1, 3) * this(2, 2)
    )
    detMat.setElement(2, 0, 
    this(1, 0) * this(2, 1) * this(3, 3) - 
    this(1, 0) * this(2, 3) * this(3, 1) - 
    this(2, 0) * this(1, 1) * this(3, 3) + 
    this(2, 0) * this(1, 3) * this(3, 1) + 
    this(3, 0) * this(1, 1) * this (2, 3) - 
    this(3, 0) * this(1, 3) * this(2, 1)
    )
    detMat.setElement(3, 0, 
    -this(1, 0) * this(2, 1) * this(3, 2) + 
    this(1, 0) * this(2, 2) * this(3, 1) + 
    this(2, 0) * this(1, 1) * this(3, 2) - 
    this(2, 0) * this(1, 2) * this(3, 1) - 
    this(3, 0) * this(1, 1) * this (2, 2) + 
    this(3, 0) * this(1, 2) * this(2, 1)
    )
    detMat.setElement(0, 1, 
    -this(0, 1) * this(2, 2) * this(3, 3) + 
    this(0, 1) * this(2, 3) * this(3, 2) + 
    this(2, 1) * this(0, 2) * this(3, 3) - 
    this(2, 1) * this(0, 3) * this(3, 2) - 
    this(3, 1) * this(0, 2) * this (2, 3) + 
    this(3, 1) * this(0, 3) * this(2, 2)
    )
    detMat.setElement(1, 1, 
    this(0, 0) * this(2, 2) * this(3, 3) - 
    this(0, 0) * this(2, 3) * this(3, 2) - 
    this(2, 0) * this(0, 2) * this(3, 3) + 
    this(2, 0) * this(0, 3) * this(3, 2) + 
    this(3, 0) * this(0, 2) * this (2, 3) - 
    this(3, 0) * this(0, 3) * this(2, 2)
    )
    detMat.setElement(2, 1, 
    -this(0, 0) * this(2, 1) * this(3, 3) + 
    this(0, 0) * this(2, 3) * this(3, 1) + 
    this(2, 0) * this(0, 1) * this(3, 3) - 
    this(2, 0) * this(0, 3) * this(3, 1) - 
    this(3, 0) * this(0, 1) * this (2, 3) + 
    this(3, 0) * this(0, 3) * this(2, 1)
    )
    detMat.setElement(3, 1, 
    this(0, 0) * this(2, 1) * this(3, 2) - 
    this(0, 0) * this(2, 2) * this(3, 1) - 
    this(2, 0) * this(0, 1) * this(3, 2) + 
    this(2, 0) * this(0, 2) * this(3, 1) + 
    this(3, 0) * this(0, 1) * this (2, 2) - 
    this(3, 0) * this(0, 2) * this(2, 1)
    )
    detMat.setElement(0, 2, 
    this(0, 1) * this(1, 2) * this(3, 3) - 
    this(0, 1) * this(1, 3) * this(3, 2) - 
    this(1, 1) * this(0, 2) * this(3, 3) + 
    this(1, 1) * this(0, 3) * this(3, 2) + 
    this(3, 1) * this(0, 2) * this (1, 3) - 
    this(3, 1) * this(0, 3) * this(1, 2)
    )
    detMat.setElement(1, 2, 
    -this(0, 0) * this(1, 2) * this(3, 3) + 
    this(0, 0) * this(1, 3) * this(3, 2) + 
    this(1, 0) * this(0, 2) * this(3, 3) - 
    this(1, 0) * this(0, 3) * this(3, 2) - 
    this(3, 0) * this(0, 2) * this (1, 3) + 
    this(3, 0) * this(0, 3) * this(1, 2)
    )
    detMat.setElement(2, 2, 
    this(0, 0) * this(1, 1) * this(3, 3) - 
    this(0, 0) * this(1, 3) * this(3, 1) - 
    this(1, 0) * this(0, 1) * this(3, 3) + 
    this(1, 0) * this(0, 3) * this(3, 1) + 
    this(3, 0) * this(0, 1) * this (1, 3) - 
    this(3, 0) * this(0, 3) * this(1, 1)
    )
    detMat.setElement(3, 2, 
    -this(0, 0) * this(1, 1) * this(3, 2) + 
    this(0, 0) * this(1, 2) * this(3, 1) + 
    this(1, 0) * this(0, 1) * this(3, 2) - 
    this(1, 0) * this(0, 2) * this(3, 1) - 
    this(3, 0) * this(0, 1) * this (1, 2) + 
    this(3, 0) * this(0, 2) * this(1, 1)
    )
    detMat.setElement(0, 3, 
    -this(0, 1) * this(1, 2) * this(2, 3) + 
    this(0, 1) * this(1, 3) * this(2, 2) + 
    this(1, 1) * this(0, 2) * this(2, 3) - 
    this(1, 1) * this(0, 3) * this(2, 2) - 
    this(2, 1) * this(0, 2) * this (1, 3) + 
    this(2, 1) * this(0, 3) * this(1, 2)
    )
    detMat.setElement(1, 3, 
    this(0, 0) * this(1, 2) * this(2, 3) -
    this(0, 0) * this(1, 3) * this(2, 2) - 
    this(1, 0) * this(0, 2) * this(2, 3) + 
    this(1, 0) * this(0, 3) * this(2, 2) + 
    this(2, 0) * this(0, 2) * this (1, 3) - 
    this(2, 0) * this(0, 3) * this(1, 2)
    )
    detMat.setElement(2, 3, 
    -this(0, 0) * this(1, 1) * this(2, 3) +
    this(0, 0) * this(1, 3) * this(2, 1) + 
    this(1, 0) * this(0, 1) * this(2, 3) - 
    this(1, 0) * this(0, 3) * this(2, 1) - 
    this(2, 0) * this(0, 1) * this (1, 3) + 
    this(2, 0) * this(0, 3) * this(1, 1)
    )
    detMat.setElement(3, 3, 
    this(0, 0) * this(1, 1) * this(2, 2) -
    this(0, 0) * this(1, 2) * this(2, 1) - 
    this(1, 0) * this(0, 1) * this(2, 2) + 
    this(1, 0) * this(0, 2) * this(2, 1) + 
    this(2, 0) * this(0, 1) * this (1, 3) - 
    this(2, 0) * this(0, 2) * this(1, 1)
    )

    var det = this(0, 0) * detMat(0, 0) + this(0, 1) * detMat(1, 0) + this(0, 2) * detMat(2, 0) + this(0, 3) * detMat(3, 0) 
    if (det == 0) {
      throw new Exception("Matrix has no inverse")
      Mat4()
    } else {
      var inverse = Mat4()
      det = 1.0 / det
      for (i <- 0 until 4) {
        for (j <- 0 until 4) {
          inverse.setElement(i, j, detMat(i, j) * det)
        }
      }
      inverse
    }
  }
}

object Mat4 {
  def apply() = {
    new Mat4
  }
  def apply(array: Array[Array[Double]]) = {
    val mat = new Mat4
    mat.values = array
    mat
  }
  def apply(v1: Vec4, v2: Vec4, v3: Vec4, v4: Vec4) = {
    val mat = new Mat4
    var i = 0
    for (v <- Array(v1, v2, v3, v4)) {
      for (j <- 0 until 4) {
        mat(j, i) = v(j)
      }
      i += 1
    }
  }
  def identity() = {
    Mat4(Array(
      Array(1.0,0.0,0.0,0.0),
      Array(0.0,1.0,0.0,0.0),
      Array(0.0,0.0,1.0,0.0),
      Array(0.0,0.0,0.0,1.0)
    ))
  }

}