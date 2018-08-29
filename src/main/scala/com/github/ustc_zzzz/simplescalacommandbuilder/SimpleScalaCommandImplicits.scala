package com.github.ustc_zzzz.simplescalacommandbuilder

import com.github.ustc_zzzz.simplescalacommandbuilder.SimpleScalaCommandExecutor._

import scala.language.implicitConversions
import scala.util.{Success, Try}

/**
  * @author ustc_zzzz
  */
object SimpleScalaCommandImplicits {
  case class SeqTransformer[+V, -T, -U] private(f: (T, U) => Try[V]) {
    def apply(t: Try[T], u: Try[U]): Try[V] = for (tt <- t; uu <- u; v <- f(tt, uu)) yield v
  }

  trait SeqTransformLowerLowerLowerImplicits {
    implicit def seqTransform0[A, B]: SeqTransformer[(A, B), A, B] = SeqTransformer((a, b) => Success(a, b))
  }

  trait SeqTransformLowerLowerImplicits extends SeqTransformLowerLowerLowerImplicits {
    // TODO: flatten all the 22 tuples

    implicit def seqTransform6[A, B, C, D, E, F]: SeqTransformer[(A, B, C, D, E, F), (A, B, C, D, E), F] = SeqTransformer((a, f) => Success(a._1, a._2, a._3, a._4, a._5, f))

    implicit def seqTransform5[A, B, C, D, E]: SeqTransformer[(A, B, C, D, E), (A, B, C, D), E] = SeqTransformer((a, e) => Success(a._1, a._2, a._3, a._4, e))

    implicit def seqTransform4[A, B, C, D]: SeqTransformer[(A, B, C, D), (A, B, C), D] = SeqTransformer((a, d) => Success(a._1, a._2, a._3, d))

    implicit def seqTransform3[A, B, C]: SeqTransformer[(A, B, C), (A, B), C] = SeqTransformer((a, c) => Success(a._1, a._2, c))
  }

  trait SeqTransformLowerImplicits extends SeqTransformLowerLowerImplicits {
    implicit def seqTransform2[A]: SeqTransformer[A, A, Unit] = SeqTransformer((a, _) => Success(a))
  }

  trait SeqTransformImplicits extends SeqTransformLowerImplicits {
    implicit def seqTransform1[A]: SeqTransformer[A, Unit, A] = SeqTransformer((_, a) => Success(a))
  }

  trait TransformImplicits {
    implicit def transform[T](arg: SingleArg[T]): ListArg[T] = new FromSingle[T](arg)

    implicit def transformOption[T](arg: SingleArg[Option[T]]): ListArg[T] = new FromOption[T](arg)
  }
}
