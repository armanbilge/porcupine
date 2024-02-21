/*
 * Copyright 2023 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package porcupine

import cats.Applicative
import cats.ContravariantMonoidal
import cats.InvariantMonoidal
import cats.data.{State, StateT}
import cats.syntax.all.*
import scodec.bits.ByteVector
import scala.deriving.Mirror

trait Encoder[A]:
  outer =>

  def parameters: Int

  def encode(a: A): List[LiteValue]

//  def either[B](right: Encoder[B]): Encoder[Either[A, B]] = new:
//    TODO figure out if this is reasonably implementable
//    def parameters: Int = ???
//
//    def encode(aorb: Either[A, B]) = aorb match
//      case Left(a) => outer.encode(a)
//      case Right(b) => right.encode(b)

  def opt: Encoder[Option[A]] =
//    either(Codec.`null`).contramap(_.toLeft(None))
    new:
      def parameters = outer.parameters
      def encode(aopt: Option[A]) = aopt match
        case None => Codec.`null`.encode(None)
        case Some(a) => outer.encode(a)

object Encoder:
  given ContravariantMonoidal[Encoder] = new:
    def unit = Codec.unit

    def product[A, B](fa: Encoder[A], fb: Encoder[B]) = new:
      def parameters =
        fa.parameters + fb.parameters

      def encode(ab: (A, B)) =
        val (a, b) = ab
        fa.encode(a) ::: fb.encode(b)

    def contramap[A, B](fa: Encoder[A])(f: B => A) = new:
      def parameters = fa.parameters
      def encode(b: B) = fa.encode(f(b))

trait Decoder[A]:
  outer =>

  def decode: StateT[Either[Throwable, *], List[LiteValue], A]

  def or[AA >: A](other: Decoder[AA]): Decoder[AA] = new:
    def decode = outer.decode.widen[AA].handleErrorWith(_ => other.decode)

  def either[B](right: Decoder[B]): Decoder[Either[A, B]] =
    outer.map(Left(_)).or(right.map(Right(_)))

  def opt: Decoder[Option[A]] =
    outer.map(Some(_)).or(Codec.`null`.asDecoder.as(None))

object Decoder:
  extension [H](head: Decoder[H])
    def *:[T <: Tuple](tail: Decoder[T]): Decoder[H *: T] = (head, tail).mapN(_ *: _)

  extension [A <: Tuple](fa: Decoder[A])
    def pmap[P <: Product](using
        m: Mirror.ProductOf[P] { type MirroredElemTypes = A },
    ): Decoder[P] = fa.map(m.fromProduct(_))

  given Applicative[Decoder] = new:
    def pure[A](a: A) = new:
      def decode = StateT.pure(a)

    def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]) = new:
      def decode = ff.decode.ap(fa.decode)

    override def product[A, B](fa: Decoder[A], fb: Decoder[B]) = new:
      def decode = fa.decode.product(fb.decode)

    override def map[A, B](fa: Decoder[A])(f: A => B) = new:
      def decode = fa.decode.map(f)

trait Codec[A] extends Encoder[A], Decoder[A]:
  outer =>

  def asEncoder: Encoder[A] = this
  def asDecoder: Decoder[A] = this

//  def either[B](right: Codec[B]): Codec[Either[A, B]] = new:
//    def parameters: State[Int, String] =
//      outer.asEncoder.either(right).parameters
//
//    def encode(aorb: Either[A, B]) =
//      outer.asEncoder.either(right).encode(aorb)
//
//    def decode = outer.asDecoder.either(right).decode

  override def opt: Codec[Option[A]] = new:
    def parameters = outer.parameters
    def encode(aopt: Option[A]) = outer.asEncoder.opt.encode(aopt)
    def decode = outer.asDecoder.opt.decode

object Codec:
  extension [H](head: Codec[H])
    def *:[T <: Tuple](tail: Codec[T]): Codec[H *: T] = (head, tail).imapN(_ *: _) {
      case h *: t => (h, t)
    }

  private final class Simple[T](
      name: String,
      apply: T => LiteValue,
      unapply: PartialFunction[LiteValue, T],
  ) extends Codec[T] {
    override def parameters: Int = 1
    override def encode(a: T): List[LiteValue] = apply(a) :: Nil
    override def decode: StateT[Either[Throwable, *], List[LiteValue], T] = StateT {
      case unapply(l) :: tail => Right((tail, l))
      case other => Left(new RuntimeException(s"Expected $name, got ${other.headOption}"))
    }
  }

  val integer: Codec[Long] =
    new Simple("integer", LiteValue.Integer.apply, { case LiteValue.Integer(i) => i })

  val real: Codec[Double] =
    new Simple("real", LiteValue.Real.apply, { case LiteValue.Real(r) => r })

  val text: Codec[String] =
    new Simple("text", LiteValue.Text.apply, { case LiteValue.Text(t) => t })

  val blob: Codec[ByteVector] =
    new Simple("blob", LiteValue.Blob.apply, { case LiteValue.Blob(b) => b })

  val `null`: Codec[None.type] =
    new Simple("NULL", _ => LiteValue.Null, { case LiteValue.Null => None })

  def unit: Codec[Unit] = new:
    def parameters: Int = 0
    def encode(u: Unit) = Nil
    def decode = StateT.pure(())

  def nil: Codec[EmptyTuple] = unit.imap(_ => EmptyTuple)(_ => ())

  given InvariantMonoidal[Codec] = new:
    def unit = Codec.unit

    def product[A, B](fa: Codec[A], fb: Codec[B]) = new:
      def parameters =
        fa.parameters + fb.parameters

      def encode(ab: (A, B)) =
        val (a, b) = ab
        fa.encode(a) ::: fb.encode(b)

      def decode = fa.decode.product(fb.decode)

    def imap[A, B](fa: Codec[A])(f: A => B)(g: B => A) = new:
      def parameters = fa.parameters
      def encode(b: B) = fa.encode(g(b))
      def decode = fa.decode.map(f)
