package com.github.kmizu.polymorphic_record

/**
  * @author Kota Mizushima
  */
object PolymorphicRecord {
  type Name = String
  type Label = String
  type Subst = Map[Name, Type]

  sealed abstract class Expression
  case class EVar(name: Name) extends Expression
  case class EPrim(op: Prim) extends Expression
  case class EApp(f: Expression, arg: Expression) extends Expression
  case class ELet(name: Name, value: Expression, body: Expression) extends Expression

  sealed abstract class Prim
  case class PInt(value: Int) extends Prim
  case class PBool(value: Boolean) extends Prim
  case object Condition extends Prim
  case class RecordSelect(label: Label) extends Prim
  case class RecordRestrict(label: Label) extends Prim

  sealed abstract class Type
  case class TVar(name: Name) extends Type
  case object TInt extends Type
  case object TBool extends Type
  case class TFun(f: Type, arg: Type) extends Type
  case class TRecord(t: Type) extends Type
  case object TRowEmpty extends Type
  case class TRowExtend(label: Label, t1: Type, t2: Type) extends Type

  case class Scheme(tvs: List[Name], t: Type)

  sealed trait Types[A] {
    def ftv(a: A): Set[Name]
    def apply: Subst => A => A
  }

  implicit object TypesType extends Types[Type] {
    override def ftv(t: Type): Set[Name] = t match {
      case TVar(n) => Set(n)
      case TInt => Set.empty
      case TBool => Set.empty
      case TFun(t1, t2) => ftv(t1) union ftv(t2)
      case TRecord(t) => ftv(t)
      case TRowEmpty => Set.empty
      case TRowExtend(l, t, r) => ftv(r) union ftv(t)
    }

    override def apply: Subst => Type => Type = s => t => t match {
      case TVar(n) =>
        s.get(n) match {
          case None => TVar(n)
          case Some(t) => t
        }
      case TFun(t1, t2) =>
        TFun(apply(s)(t1), apply(s)(t2))
      case TRecord(t) =>
        TRecord(apply(s)(t))
      case TRowExtend(l, t, r) =>
        TRowExtend(l, apply(s)(t), apply(s)(r))
      case t => t
    }
  }

  implicit def TypesScheme(implicit ev: Types[Type]): Types[Scheme] = new Types[Scheme] {
    override def ftv(scheme: Scheme): Set[Name] = scheme match {
      case Scheme(vars, t) => ev.ftv(t) diff Set(vars:_*)
    }

    override def apply: Subst => Scheme => Scheme = s => scheme => scheme match {
      case Scheme(vars, t) => Scheme(vars, ev.apply(vars.foldRight(s){(n, m) => m - n})(t))
    }
  }

  implicit def TypesList[A](implicit ev: Types[A]): Types[List[A]] = new Types[List[A]] {
    override def ftv(l: List[A]): Set[Name] = {
      l.map(ev.ftv).foldRight(Set.empty[Name]){(a, b) => b union b}
    }

    override def apply: Subst => List[A] => List[A] = s => {
      _.map(ev.apply(s))
    }
  }

  val nullSubst : Subst = Map.empty

  def composeSubst(s1: Subst, s2: Subst)(implicit ev: Types[Type]): Subst =  {
    s2.mapValues(ev.apply(s1)) ++ s1
  }

  case class TypeEnv(map: Map[Name, Scheme])

  def remove(env: TypeEnv, name: Name): TypeEnv = env match {
    case TypeEnv(env) => TypeEnv(env - name)
  }

  implicit def TypesTypeEnv(implicit ev1: Types[List[Scheme]], ev2: Types[Scheme]): Types[TypeEnv] = new Types[TypeEnv] {
    override def ftv(env: TypeEnv): Set[Name] = env match {
      case TypeEnv(env) => ev1.ftv(env.values.toList)
    }

    override def apply: (Subst) => (TypeEnv) => TypeEnv = s => env => env match {
      case TypeEnv(env) => TypeEnv(env.mapValues(ev2.apply(s)))
    }
  }

  def generalize(env: TypeEnv, t: Type)(implicit ev1: Types[Type], ev2: Types[TypeEnv]): Scheme = {
    val vars = (ev1.ftv(t) diff ev2.ftv(env)).toList
    Scheme(vars, t)
  }
}
