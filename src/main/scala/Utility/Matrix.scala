package utility

import Vec._

class Mat33 {
  var values = Array.ofDim[Double](3,3)

  def +(that: Mat33) = {
    var result = Mat33()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        result.setElement(i, j, that(i, j) + this(i, j)) 
      }
    }
    result
  }

  def *(that: Mat33) = {
    var result = Mat33()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        val sumProd = this(i, 0)*that(0, j) + this(i, 1)*that(1, j) + this(i, 2)*that(2 ,j)
        result.setElement(i, j, sumProd) 
      }
    }
    result
  }

  def *(t: Double) = {
    var result = Mat33()
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
    var result = Mat33()
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        result.setElement(i, j, this(j, i)) 
      }
    }
    result
  }

  def apply(i: Int, j: Int) = {
    this.values(i)(j)
  }

  def setElement(i: Int, j: Int, el: Double) = {
    this.values(i)(j) = el
  }
}

object Mat33 {
  def apply() = {
    new Mat33
  }
  def apply(array: Array[Array[Double]]) = {
    val mat = new Mat33
    mat.values = array
    mat
  }

}

case class Mat44() {
  var values = Array.ofDim[Double](4,4)

  def +(that: Mat44) = {
    var result = Mat44()
    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        result.setElement(i, j, that(i, j) + this(i, j)) 
      }
    }
    result
  }

  def *(that: Mat44) = {
    var result = Mat44()
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
    var result = Mat44()
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
    var result = Mat44()
    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        result.setElement(i, j, this(j, i)) 
      }
    }
    result
  }

  def apply(i: Int, j: Int) = {
    this.values(i)(j)
  }

  def setElement(i: Int, j: Int, el: Double) = {
    this.values(i)(j) = el
  }

  // Ugly but efficient
  def inverse(): Mat44 = {
    var detMat = Mat44()
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
      throw new IllegalArgumentException("Matrix has no inverse")
      Mat44()
    } else {
      var inverse = Mat44()
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

object Mat44 {
  def apply() = {
    new Mat44
  }
  def apply(array: Array[Array[Double]]) = {
    val mat = new Mat44
    mat.values = array
    mat
  }
  def identity() = {
    Mat44(Array(
      Array(1.0,0.0,0.0,0.0),
      Array(0.0,1.0,0.0,0.0),
      Array(0.0,0.0,1.0,0.0),
      Array(0.0,0.0,0.0,1.0)
    ))
  }

}