
package tuplicity

import reflect.api.Universe
import language.experimental.macros
import reflect.macros.Context

import java.util.NoSuchElementException

private[tuplicity] object FormatInfoMacros {

	def typeNames(c: Context)(typeList: List[c.Type]): c.Expr[List[String]] = {
		import c.universe._

		val nameList = typeList.map { tpe =>
			val name = tpe.typeSymbol.fullName
			Literal(Constant(name))
		}

		val listCompanionIdent = Ident(typeOf[List[_]].typeSymbol.companionSymbol)
		c.Expr[List[String]](Apply(listCompanionIdent, nameList))

	}

	def primitiveValueTypeNames(c: Context): c.Expr[List[String]] =
		typeNames(c)(FormatInfo.primitiveValuePackingOrder(c.universe))

	def fakeMemberTypeNames(c: Context): c.Expr[List[String]] =
		typeNames(c)(FormatInfo.fakeMemberMappings(c.universe).map(_._1))

	}

	private[tuplicity] object FormatInfo {

	def primitiveValuePackingOrder(u: Universe) = List(
			u.typeOf[Boolean],
			u.typeOf[Byte],
			u.typeOf[Char],
			u.typeOf[Short],
			u.typeOf[Int],
			u.typeOf[Long],
			u.typeOf[Float],
			u.typeOf[Double]
	)

	def fakeMemberMappings(u: Universe) = List(
		u.typeOf[Unit] -> u.Literal(u.Constant(()))
	)

	def fakeMembers(u: Universe) = fakeMemberMappings(u).map(_._1)

	def fakeMemberValue(u: Universe) = { (t: u.Type) =>
		fakeMemberMappings(u).find(_._1 =:= t).get._2
	}

	def primitiveValueTypeNames = macro FormatInfoMacros.primitiveValueTypeNames

	def fakeMemberTypeNames = macro FormatInfoMacros.fakeMemberTypeNames

	def fakeMemberTypes(u: Universe) = fakeMemberMappings(u).map(_._1)

}

