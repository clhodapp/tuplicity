
import org.scalatest.FunSuite

import tuplicity._

class TupleSuite extends FunSuite {

	test("Access contents of single-value-type Tuples") {
		val unitTuple = Tuple(())
		val booleanTuple = Tuple((false: Boolean))
		val byteTuple = Tuple((1: Byte))
		val charTuple = Tuple((1: Char))
		val shortTuple = Tuple(1: Short)
		val intTuple = Tuple(1: Int)
		val longTuple = Tuple(1: Long)
		val floatTuple = Tuple(1: Float)
		val doubleTuple = Tuple(1: Double)
		assert(unitTuple._1 === ())
		assert(booleanTuple._1 === false)
		assert(byteTuple._1 === (1: Byte))
		assert(charTuple._1 === (1: Char))
		assert(shortTuple._1 === (1: Short))
		assert(intTuple._1 === (1: Int))
		assert(longTuple._1 === (1: Long))
		assert(floatTuple._1 === (1: Float))
		assert(doubleTuple._1 === (1: Double))
	}

	test("Access contents of multi-value-type Tuples") {
		val t = Tuple(
			(),
			false: Boolean,
			1: Byte,
			2: Char,
			3: Short,
			4: Int,
			5: Long,
			6: Float,
			7: Double
		)
		assert(t._1 === ())
		assert(t._2 === (false: Boolean))
		assert(t._3 === (1: Byte))
		assert(t._4 === (2: Char))
		assert(t._5 === (3: Short))
		assert(t._6 === (4: Int))
		assert(t._7 === (5: Long))
		assert(t._8 === (6: Float))
		assert(t._9 === (7: Double))
	}

	test("Access contents of single-reference-item Tuple") {
		val t = Tuple("a")
		assert(t._1 === "a")
	}

	test("Access contents of multi-reference-item Tuple") {
		val t = Tuple("a", "b", "c", List(1,2))
		assert(t._1 === "a")
		assert(t._2 === "b")
		assert(t._3 === "c")
		assert(t._4 === List(1,2))
	}

	test("Access contents of mixed value/reference Tuple") {
		val t = Tuple(
			(),
			false: Boolean,
			1: Char,
			"a",
			2: Int,
			3: Long,
			List(1,2,3),
			4: Float,
			"b",
			(null: String),
			5: Double
		)

		assert(t._1 === ())
		assert(t._2 === (false: Boolean))
		assert(t._3 === (1: Char))
		assert(t._4 === "a")
		assert(t._5 === (2: Int))
		assert(t._6 === (3: Long))
		assert(t._7 === List(1,2,3))
		assert(t._8 === (4: Float))
		assert(t._9 === "b")
		assert(t._10 === (null: String))
		assert(t._11 === (5: Double))
	}

	test("Partially compile-time stringification") {
		val t = Tuple(
			(),
			false: Boolean,
			'x': Char,
			"a",
			2: Int,
			3: Long,
			List(1,2,3),
			4: Float,
			"b",
			(null: String),
			5: Double
		)

		assert(
			t.toString ===
			"((),false,x,a,2,3,List(1, 2, 3),4.0,b,null,5.0)"
		)
	}

	test("Fully run-time stringification") {
		val t: AnyRef = Tuple(
			(),
			false: Boolean,
			'x': Char,
			"a",
			2: Int,
			3: Long,
			List(1,2,3),
			4: Float,
			"b",
			(null: String),
			5: Double
		)

		assert(
			t.toString ===
			"((),false,x,a,2,3,List(1, 2, 3),4.0,b,null,5.0)"
		)
	}

	test("Covariance forcing fully runtime gets") {
		val t: Tuple[Unit~Any~AnyVal~AnyRef~Int~AnyVal~List[Int]~Float~String~Any~Double] = Tuple(
			(),
			false: Boolean,
			'x': Char,
			"a",
			2: Int,
			3: Long,
			List(1,2,3),
			4: Float,
			"b",
			(null: String),
			5: Double
		)

		assert(t._1 === ())
		assert(t._2 === (false: Any))
		assert(t._3 === ('x': AnyVal))
		assert(t._4 === ("a": AnyRef))
		assert(t._5 === (2: Int))
		assert(t._6 === (3: AnyVal))
		assert(t._7 === (List(1,2,3): List[Int]))
		assert(t._8 === (4: Float))
		assert(t._9 === ("b": String))
		assert(t._10 === (null: Any))
		assert(t._11 === (5: Double))

		assert(
			t.toString ===
			"((),false,x,a,2,3,List(1, 2, 3),4.0,b,null,5.0)"
		)
	}

	test("AnyVal members"){
		val t = Tuple(
			(1: AnyVal),
			(2: Int),
			('x': AnyVal),
			('z': Char),
			"abc",
			(3.4f: AnyVal),
			(3.6f: Double),
			"def",
			(5: Int)
		)

		assert(t._1 === (1: AnyVal))
		assert(t._2 === (2: Int))
		assert(t._3 === ('x': AnyVal))
		assert(t._4 === ('z': Char))
		assert(t._5 === "abc")
		assert(t._6 === (3.4f: AnyVal))
		assert(t._7 === (3.6f: Double))
		assert(t._8 === "def")
		assert(t._9 === (5: Int))
	}

	test("Equality") {
		val t1, t2 = Tuple(
			(1: AnyVal),
			(2: Int),
			('x': AnyVal),
			('z': Char),
			"abc",
			(3.4f: AnyVal),
			(3.6f: Double),
			"def",
			(5: Int)
		)

		val t3, t4 = Tuple(
			(),
			false: Boolean,
			1: Byte,
			2: Char,
			3: Short,
			4: Int,
			5: Long,
			6: Float,
			7: Double
		)

		val unitTuple1, unitTuple2 = Tuple(())
		val booleanTuple1, booleanTuple2 = Tuple((false: Boolean))
		val byteTuple1, byteTuple2 = Tuple((1: Byte))
		val charTuple1, charTuple2 = Tuple((1: Char))
		val shortTuple1, shortTuple2 = Tuple(1: Short)
		val intTuple1, intTuple2 = Tuple(1: Int)
		val longTuple1, longTuple2 = Tuple(1: Long)
		val floatTuple1, floatTuple2 = Tuple(1: Float)
		val doubleTuple1, doubleTuple2 = Tuple(1: Double)

		assert(t1 === t2)
		assert(t3 === t4)
		assert(booleanTuple1 === booleanTuple2)
		assert(unitTuple1 === unitTuple2)
		assert(byteTuple1 === byteTuple2)
		assert(charTuple1 === charTuple2)
		assert(shortTuple1 === shortTuple2)
		assert(intTuple1 === intTuple2)
		assert(longTuple1 === longTuple2)
		assert(floatTuple1 === floatTuple2)
		assert(doubleTuple1 === doubleTuple2)
	}

}

