package utility

import org.scalatest.funspec.AnyFunSpec  

import utility.Mat44

class MatrixTest extends AnyFunSpec {
  describe("Mat44 functions") {
    it("calculates correct inverse") {
      val mat = Mat44(Array(
        Array(2.0, 0.0, 10.0, 0.0),
        Array(1.0, 3.0, 0.0, 0.0),
        Array(0.0, 0.0, 4.0, 0.0),
        Array(0.0, 0.0, 0.0, 5.0),
      ))
      assert(mat * mat.inverse == Mat44.identity())
    }
  }
}