
package tuplicity

import language.experimental.macros
import language.dynamics
import language.postfixOps
import language.existentials
import scala.reflect.macros.Context

import java.util.regex.Pattern

sealed trait ~[+A, +B]

package internal {
	object TupleHelpers {
		private[tuplicity] val primitiveValueTypeNames = FormatInfo.primitiveValueTypeNames
			private[tuplicity] val typeSeparator = "#X#$#X#"
			def stringify(a: Any): String = a match {
				case null => "null"
				case a: Array[_] => a.deep.toString
					case x => x.toString
			}
	}
}

private[tuplicity] abstract class TupleValueExtractor {

	// Be warned: This code abstracts over the
	// macro reflection API and the tuplicity-specific
	// runtime reflection micro-API. It recycles
	// the name `Type` from the standard library
	// reflection API, but it does not have the
	// quite same meaning here as it does there,
	// as it is a stand-in for whatever we are
	// using to represent types in this extractor,
	// and imposes no actual contract.

	type Type
	type Data
	type Intermediate
	type Result
	type Layout = Seq[Type]

	def generalize(t: Type): Type
	def sameType(t1: Type, t2: Type): Boolean

	val primitivesInOrder: Seq[Type]
	val fakeMemberTypes: Seq[Type]

	val data: Data
	val layout: Layout

	def fakeMemberValue(fakeMember: Type): Result

	def liftedData(tpe: Type): Result

	def liftedIntermediate(i: Intermediate, tpe: Type): Result

	def topLevel(index: Int): Intermediate

	def secondLevel(i: Intermediate, index: Int, resultTpe: Type): Result

	def isFake(t: Type) =
		fakeMemberTypes.exists(f => sameType(f, t))

	def isPrimitive(t: Type) =
		primitivesInOrder.exists(p => sameType(generalize(t), generalize(p)))

	def isReference(t: Type) = !(isPrimitive(t) || isFake(t))

	def partialCounts(test: Type => Boolean) =
		layout.scanLeft(0) { (sum: Int, tpe: Type) =>
			if (test(tpe)) sum + 1
			else sum
		}

	lazy val numFakesBefore = partialCounts(isFake)

	lazy val numFakeTypes = numFakesBefore.last

	lazy val numPrimitivesBefore = partialCounts(isPrimitive)

	lazy val primitivesPresent =
		primitivesInOrder.map(p => layout.exists(t => sameType(generalize(t), p)))


	def get(index: Int): Result = {
		val typeOfSelected = layout(index)
		if (fakeMemberTypes.exists(sameType(_, typeOfSelected)))
			fakeMemberValue(typeOfSelected)
		else if (primitivesInOrder.exists(sameType(_, generalize(typeOfSelected))))
			extractPrimitive(index)
		else
			extractReference(index)
	}

	def extractPrimitive(index: Int): Result = {

		val typeOfSelected = layout(index)

		if (layout.count(t => !sameType(generalize(t), generalize(typeOfSelected))) == numFakeTypes) {
			liftedIntermediate(topLevel(index - numFakesBefore(index)), typeOfSelected)
		} else {
			val indexInPrims = primitivesInOrder.indexWhere(sameType(_, generalize(typeOfSelected)))
			val indexInTopLevel = primitivesPresent.take(indexInPrims).count(identity)
			val intermediate = topLevel(indexInTopLevel)
			val indexInSecondLevel =
				layout.take(index).count(t => sameType(generalize(t), generalize(typeOfSelected)))
			secondLevel(intermediate, indexInSecondLevel, typeOfSelected)
		}
	}

	def extractReference(index: Int): Result = {
		val typeOfSelected = layout(index)
		if (layout.count(t => !isFake(t)) == 1) {
			liftedData(typeOfSelected)
		} else {
			val indexInTopLevel =
				index +
				primitivesPresent.count(identity) -
				numPrimitivesBefore(index) -
				numFakesBefore(index)
			val intermediate = topLevel(indexInTopLevel)
			liftedIntermediate(intermediate, typeOfSelected)
		}
	}

}

private[tuplicity] abstract class RuntimeExtractor(val data: AnyRef, val layout: Seq[String]) extends TupleValueExtractor {
	type Type = String
	type Data = AnyRef
	type Intermediate = Any

	val primitivesInOrder = FormatInfo.primitiveValueTypeNames
	val fakeMemberTypes = FormatInfo.fakeMemberTypeNames

	def generalize(t: Type): Type = t
	def sameType(t1: Type, t2: Type): Boolean = t1 == t2

	def topLevel(index: Int) = data.asInstanceOf[Array[_]](index)

}

private[tuplicity] class RuntimeStringExtractor(data: AnyRef, layout: Seq[String]) extends RuntimeExtractor(data, layout) {

	import internal.TupleHelpers._

	type Result = String

	def fakeMemberValue(fakeMember: Type) = "()"

	def liftedData(tpe: Type) = stringify(data)

	def liftedIntermediate(i: Intermediate, tpe: Type) = stringify(i)

	def secondLevel(i: Intermediate, index: Int, resultTpe: Type) = stringify(i.asInstanceOf[Array[_]](index))

}

private[tuplicity] class RuntimeAnyExtractor(data: AnyRef, layout: Seq[String]) extends RuntimeExtractor(data, layout) {

	type Result = Any

	def fakeMemberValue(fakeMember: Type) = ()

	def liftedData(tpe: Type) = data: Any

	def liftedIntermediate(i: Intermediate, tpe: Type) = i: Any

	def secondLevel(i: Intermediate, index: Int, resultTpe: Type) = i.asInstanceOf[Array[_]](index): Any

}

// Hack to get around Scala's issues with dependant types in constructors
private[tuplicity] object TreeExtractor {
	def apply[T](c: Context { type PrefixType <: Tuple[T] })(layout: Seq[c.Type]) = {

		import c.universe.{Type=>CType,_}

		val arrayIdent = Ident(typeOf[Array[_]].typeSymbol)
		val existentialArrayType = typeOf[Array[_]]

		val outerLayout = layout

		class TreeExtractor extends TupleValueExtractor {

			type Type = CType
			type Data = Context { type PrefixType <: Tuple[T] }
			type Intermediate = Tree
			type Result = Tree

			val data = c

			val layout = outerLayout

			def generalize(t: CType) = t.erasure

			def sameType(t1: CType, t2: CType) = t1 =:= t2

			val primitivesInOrder = FormatInfo.primitiveValuePackingOrder(c.universe)

			val fakeMemberTypes = FormatInfo.fakeMemberTypes(c.universe)

			def fakeMemberValue(fakeMember: CType) = Literal(Constant(()))

			def cast(t: Tree, tpe: Tree) = TypeApply(Select(t, newTermName("asInstanceOf")), List(tpe))

			def liftedData(tpe: CType) = cast(reify { c.prefix.splice.data }.tree, TypeTree(tpe))

			def liftedIntermediate(i: Tree, tpe: CType) = cast(i, TypeTree(tpe))

			def topLevel(index: Int) = {
				
				val indexTree = Literal(Constant(index))
				val prefixTree = c.prefix.tree
				val dataTree = Select(prefixTree, newTermName("data"))
				val arrayTree = cast(dataTree, TypeTree(existentialArrayType))
				Apply(Select(arrayTree, newTermName("apply")), List(indexTree))
			}

			def secondLevel(i: Tree, index: Int, resultTpe: CType) = {
				val arrayTree = cast(i, AppliedTypeTree(arrayIdent, List(TypeTree(resultTpe))))
				Apply(Select(arrayTree, newTermName("apply")), List(Literal(Constant(index))))
			}

			val runtimeForcingTypes = fakeMemberTypes.flatMap(_.baseClasses).map(_.asType.toType)

			def getAtRuntime(index: Int) = {
				val typeOfSelected = layout(index)
				Apply(
					TypeApply(
						Select(c.prefix.tree, newTermName("runtimeGet")),
						List(TypeTree(typeOfSelected))
					),
					List(Literal(Constant(index)))
				)
			}

			override def get(index: Int) = {
				if (layout.exists(t1 => runtimeForcingTypes.exists(t2 => sameType(t1, t2)))) getAtRuntime(index)
				else super.get(index)
			}
		}
		new TreeExtractor
	}
}

private[tuplicity] object TupleMacros {

	import internal.TupleHelpers._
	import FormatInfo._

	private[this] final def listify[T: c.WeakTypeTag](c: Context): List[c.universe.Type] = {
		import c.universe._
		val tpe = weakTypeOf[T]
		val tildeType = weakTypeOf[~[_, _]].typeConstructor
		def isTildeType(tpe: Type) = tpe.erasure =:= tildeType.erasure
		def helper(tpe: Type, current: List[Type]): List[Type] = {
			tpe match {
				case TypeRef(_, _, List(first, second)) if isTildeType(tpe) => helper(first, second :: current)
				case end => end :: current
			}
		}
		helper(tpe, Nil)
	}

	def apply(c: Context)(exprs: c.Expr[Any]*): c.Expr[Tuple[_]] = {

		import c.universe._

		val primitivesInOrder = primitiveValuePackingOrder(c.universe)
		val fakeMemberTypes = FormatInfo.fakeMemberTypes(c.universe)

		val termList = exprs.map(_.tree).toList
		val typeList = termList.map(_.tpe)
		val tildedList = {
			val tildeType = typeOf[~[_,_]].typeSymbol
			val tildeIdent = Ident(tildeType)
			val trees = typeList.map(t => TypeTree(t))
			def tildeTogether(t1: Tree, t2: Tree): Tree = AppliedTypeTree(tildeIdent, List(t1, t2))
			trees.tail.foldLeft[Tree](trees.head)((t1: Tree, t2: Tree) => tildeTogether(t1, t2))
		}

		val structure = typeList.map {
			case tpe if primitivesInOrder.exists(tpe.erasure =:= _) => tpe.erasure.typeSymbol.fullName
			case tpe => tpe.typeSymbol.fullName
		}.mkString(typeSeparator)


		val fakes : List[Tree] = termList.filter(t => fakeMemberTypes.exists(f => t.tpe =:= f))
		val primitives: List[Tree] = termList.filter(t => primitivesInOrder.exists(p => t.tpe.erasure =:= p))
		val refrences: List[Tree] = (termList diff fakes) diff primitives

		def termsWithErasureMatching(tpe: Type) = termList.filter(_.tpe.erasure =:= tpe)

		val arrayIdentTree = Ident(weakTypeOf[Array[_]].typeSymbol)
		val arrayCompanionIdent = Ident(typeOf[Array[_]].typeSymbol.companionSymbol)

		def typeTree[T: TypeTag] = TypeTree(typeOf[T])

		def arrayType[T: TypeTag] = AppliedTypeTree(arrayIdentTree, List(typeTree[T]))

		def applyToArray(args: List[Tree]) = Apply(Select(arrayCompanionIdent, newTermName("apply")), args)

		def optArray[T: TypeTag](args: List[Tree], tpe: Type) =
			if (args.isEmpty) Nil
			else List(applyToArray(args))

		def buildArgument(input: List[Tree], tpe: Type): List[Tree] = {
			val matching = termsWithErasureMatching(tpe)
			optArray(matching, tpe)
		}

		val primitiveArrayTrees = primitiveValuePackingOrder(c.universe).map(buildArgument(termList, _)).flatten

		val refArrayContents = primitiveArrayTrees ::: refrences

		val tupleIdent = Ident(weakTypeOf[Tuple[_]].typeSymbol)

		def constructTupleTree(data: Tree) = {
			Apply(
				Select(New(AppliedTypeTree(tupleIdent, List(tildedList))), nme.CONSTRUCTOR),
				List(data, Literal(Constant(structure)))
			)
		}

		val dataTree = refArrayContents.size match {
			case 0 => constructTupleTree(Literal(Constant(null)))
			case 1 => constructTupleTree(refArrayContents.head)
			case _ => constructTupleTree(applyToArray(refArrayContents))
		}

		c.Expr[Tuple[_]](dataTree)

	}

	def select[T: c.WeakTypeTag](c: Context { type PrefixType <: Tuple[T] })(nameExpr: c.Expr[String]): c.Expr[Any] = {

		import c.universe._

		val IndexPattern = """_(\d+)""".r
		val tpe = implicitly[WeakTypeTag[T]].tpe
		val pos = nameExpr.tree.pos

		nameExpr.tree match {

			case Literal(Constant(name: String)) =>

				name match {
					case IndexPattern(idx) => get[T](c)(name, pos, idx.toInt)
					case _ => c.abort(pos, s"value $name is not a member of $tpe")
				}

			case _ => c.abort(pos, "Tuple#selectDynamic only accepts String literals")

		}
	}

	def get[T: c.WeakTypeTag](c: Context { type PrefixType <: Tuple[T] })(name: String, pos: c.Position, idx: Int): c.Expr[Any] = {

		import c.universe._

		val typeList = listify[T](c)

		val indexInTuple = idx.toInt - 1

		val tupleTpe = weakTypeOf[T]

		def validIndex(index: Int) = index < typeList.size && index >= 0

		if (!validIndex(indexInTuple))
			c.abort(pos, s"value $name is not a member of $tupleTpe")

		val extractor = TreeExtractor(c)(typeList)

		c.Expr[Any](extractor.get(indexInTuple))

	}

	def toString[T: c.WeakTypeTag](c: Context { type PrefixType <: Tuple[T] }) = {
		import c.universe._

		val typeList = listify[T](c)

		val extractor = TreeExtractor(c)(typeList)

		val rawExprs = for (i <- typeList.indices) yield c.Expr[Any](extractor.get(i))
		val stringExprs = rawExprs.map(t => reify(tuplicity.internal.TupleHelpers.stringify(t.splice)))
		val middleExpr = stringExprs.reduce { (t1: c.Expr[String], t2: c.Expr[String]) =>
			reify { t1.splice.concat(",").concat(t2.splice) }
		}
		reify { "(".concat(middleExpr.splice).concat(")") }
	}

	def productArity[T: c.WeakTypeTag](c: Context): c.Expr[Int] = {
		import c.universe._
		c.Expr[Int](Literal(Constant(listify[T](c).length)))
	}

}

sealed private[tuplicity] abstract class RuntimeTupleOps(val data: AnyRef, val format: String) extends Product {

	import internal.TupleHelpers._

	override def toString: String = {

		val tupleTypeNames = format.split(Pattern.quote(typeSeparator))

		val extractor = new RuntimeStringExtractor(data, tupleTypeNames)

		tupleTypeNames.indices.map { idx =>
			extractor.get(idx)
		}.mkString("(", ",", ")")
	}

	def runtimeGet[T](index: Int): T = {

		val tupleTypeNames = format.split(Pattern.quote(typeSeparator))

		val extractor = new RuntimeAnyExtractor(data, tupleTypeNames)
		extractor.get(index).asInstanceOf[T]
	}

	def productElement(n: Int): Any = runtimeGet[Any](n)

	def productArity: Int = format.split(Pattern.quote(typeSeparator)).length

	def canEqual(that: Any): Boolean = that match {
		case r: RuntimeTupleOps => true
		case _ => false
	}

	override def equals(that: Any) = that match {
		case r: RuntimeTupleOps if (r canEqual this) =>
			val selfTupleTypeNames = this.format.split(Pattern.quote(typeSeparator))
			val otherTupleTypeNames = r.format.split(Pattern.quote(typeSeparator))
			val selfExtractor = new RuntimeAnyExtractor(data, selfTupleTypeNames)
			val otherExtractor = new RuntimeAnyExtractor(r.data, otherTupleTypeNames)
			(0 until productArity).forall(i => selfExtractor.get(i) == otherExtractor.get(i))
		case _ => false
	}

}

class Tuple[+T](data: AnyRef, format: String) extends RuntimeTupleOps(data, format) with Dynamic {
	def selectDynamic(nameExpr: String) = macro TupleMacros.select[T]
	override def toString = macro TupleMacros.toString[T]
}

object Tuple {
	def apply(exprs: Any *) = macro TupleMacros.apply
}

