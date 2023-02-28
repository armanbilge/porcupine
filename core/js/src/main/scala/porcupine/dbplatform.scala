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

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import cats.syntax.all.*
import scodec.bits.ByteVector

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Uint8Array

private abstract class DatabasePlatform:
  def open[F[_]](filename: String)(implicit F: Async[F]): Resource[F, Database[F]] =
    Resource.eval(Mutex[F]).flatMap { mutex =>
      Resource
        .make(F.delay(new sqlite3.Database(filename)))(db => F.delay(db.close()))
        .evalTap(db => F.delay(db.defaultSafeIntegers(true)))
        .map { db =>
          new:
            def prepare[A, B](query: Query[A, B]): Resource[F, Statement[F, A, B]] =
              Resource
                .eval(mutex.lock.surround(F.delay(db.prepare(query.sql))))
                .evalTap(statement => F.delay(statement.raw(true)).whenA(statement.reader))
                .map { statement =>
                  def bind(args: A) = query.encoder.encode(args).map {
                    case LiteValue.Null => null
                    case LiteValue.Integer(value) => js.BigInt(value.toString)
                    case LiteValue.Real(value) => value
                    case LiteValue.Text(value) => value
                    case LiteValue.Blob(value) => value.toUint8Array
                  }

                  if statement.reader then
                    new:
                      def cursor(args: A): Resource[F, Cursor[F, B]] = mutex.lock *>
                        Resource
                          .eval {
                            F.delay(statement.iterate(bind(args)*)).map { iterator =>
                              new:
                                def fetch(maxRows: Int): F[(List[B], Boolean)] =
                                  F.delay {
                                    val rows = List.newBuilder[List[LiteValue]]
                                    var i = 0
                                    var more = true
                                    while i < maxRows && more do
                                      val entry = iterator.next()
                                      rows += entry.value.map {
                                        case null => LiteValue.Null
                                        case i if js.typeOf(i) == "bigint" =>
                                          LiteValue.Integer(i.toString.toLong)
                                        case d: Double => LiteValue.Real(d)
                                        case s: String => LiteValue.Text(s)
                                        case b: Uint8Array =>
                                          LiteValue.Blob(ByteVector.fromUint8Array(b))
                                      }.toList
                                      more = !entry.done
                                      i += 1

                                    (rows.result(), more)
                                  }.flatMap { (rows, more) =>
                                    rows
                                      .traverse(query.decoder.decode.runA(_))
                                      .tupleRight(more)
                                      .liftTo[F]
                                  }
                            }

                          }
                  else
                    args =>
                      mutex.lock *> Resource.eval {
                        F.delay(statement.run(bind(args)*)).as(_ => F.pure(Nil, false))
                      }

                }

        }
    }
