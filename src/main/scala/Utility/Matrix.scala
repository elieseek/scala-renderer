package utility

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

class Mat44 {
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

}