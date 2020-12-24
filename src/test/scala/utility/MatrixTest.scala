package utility

import org.scalatest.funspec.AnyFunSpec  

import utility.Mat4

class MatrixTest extends AnyFunSpec {
  describe("Mat4 functions") {
    it("calculates correct inverse") {
      val mat = Mat4(Array(
        Array(2.0, 0.0, 10.0, 0.0),
        Array(1.0, 3.0, 0.0, 0.0),
        Array(0.0, 0.0, 4.0, 0.0),
        Array(0.0, 0.0, 0.0, 5.0),
      ))
      assert(mat * mat.inverse == Mat4.identity())
    }
  }
  describe("Mat3 functions") {
    it("calculates correct inverse") {
      val mat = Mat3(Array(
        Array(2.0, 0.0, 10.0),
        Array(1.0, 3.0, 0.0),
        Array(0.0, 0.0, 4.0)
      ))
      assert(mat * mat.inverse == Mat3.identity())
    }
  }
}