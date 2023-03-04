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

import cats.ContravariantMonoidal
import cats.Monoid
import cats.arrow.Profunctor
import cats.syntax.all.*

final class Query[A, B](val sql: String, val encoder: Encoder[A], val decoder: Decoder[B])

object Query:

  given Profunctor[Query] = new:
    def dimap[A, B, C, D](fab: Query[A, B])(f: C => A)(g: B => D) =
      Query(fab.sql, fab.encoder.contramap(f), fab.decoder.map(g))

final class Fragment[A](val fragment: String, val encoder: Encoder[A]):
  def command: Query[A, Unit] = Query(fragment, encoder, Codec.unit)

  def query[B](decoder: Decoder[B]): Query[A, B] = Query(fragment, encoder, decoder)

  def apply(a: A): Fragment[Unit] = Fragment(fragment, encoder.contramap(_ => a))

  def stripMargin: Fragment[A] = Fragment(fragment.stripMargin, encoder)
  def stripMargin(marginChar: Char): Fragment[A] =
    Fragment(fragment.stripMargin(marginChar), encoder)

object Fragment:
  given ContravariantMonoidal[Fragment] = new:
    val unit = Fragment("", Codec.unit)
    def product[A, B](fa: Fragment[A], fb: Fragment[B]) =
      Fragment(fa.fragment + fb.fragment, (fa.encoder, fb.encoder).tupled)
    def contramap[A, B](fa: Fragment[A])(f: B => A) =
      Fragment(fa.fragment, fa.encoder.contramap(f))

  given Monoid[Fragment[Unit]] = new:
    def empty = ContravariantMonoidal[Fragment].unit
    def combine(x: Fragment[Unit], y: Fragment[Unit]) =
      (x, y).contramapN(_ => ((), ()))
