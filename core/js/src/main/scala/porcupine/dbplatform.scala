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

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

private abstract class DatabasePlatform:
  def open[F[_]](filename: String)(implicit F: Async[F]): Resource[F, Database[F]] =
    Resource.eval(Mutex[F]).flatMap { mutex =>
      Resource
        .make {
          F.async_[sqlite3.Database] { cb =>
            lazy val db: sqlite3.Database = new sqlite3.Database(
              filename,
              e => cb(Option(e).map(js.JavaScriptException(_)).toLeft(db)),
            )
            db // init
            ()
          }
        } { db =>
          mutex.lock.surround {
            F.async_[Unit] { cb =>
              db.close(e => cb(Option(e).map(js.JavaScriptException(_)).toLeft(())))
            }
          }
        }
        .map { db =>
          new:
            def prepare[A, B](query: Query[A, B]): Resource[F, Statement[F, A, B]] =
              Resource
                .make {
                  mutex.lock.surround {
                    F.async[sqlite3.Statement] { cb =>
                      F.delay {
                        lazy val statement: sqlite3.Statement =
                          db.prepare(
                            query.sql,
                            e => cb(Option(e).map(js.JavaScriptException(_)).toLeft(statement)),
                          )
                        statement // init
                        Some(F.delay(db.interrupt()))
                      }
                    }
                  }
                } { statement =>
                  mutex.lock.surround {
                    F.async_ { cb =>
                      statement.finalize(() => cb(Either.unit))
                    }
                  }
                }
                .map { statement =>
                  new:
                    def cursor(args: A): Resource[F, Cursor[F, B]] = mutex.lock *>
                      Resource
                        .make {
                          F.async[Unit] { cb =>
                            F.delay {
                              val jsArgs = query.encoder.encode(args).map {
                                case LiteValue.Null => null
                                case LiteValue.Integer(value) => value.toString
                                case LiteValue.Real(value) => value
                                case LiteValue.Text(value) => value
                                case LiteValue.Blob(value) => value.toUint8Array
                              }

                              val jscb: js.Function1[js.Error, Unit] =
                                e => cb(Option(e).map(js.JavaScriptException(_)).toLeft(()))

                              statement.bind((jsArgs ::: jscb :: Nil)*)

                              Some(F.delay(db.interrupt()))
                            }
                          }
                        }(_ => F.async_[Unit](cb => statement.reset(() => cb(Either.unit))))
                        .as {
                          new:
                            def fetch(maxRows: Int): F[(List[B], Boolean)] =
                              F.async[Option[js.Array[Any]]] { cb =>
                                F.delay {
                                  statement.get { (e, r) =>
                                    println((e, r))
                                    js.Dynamic.global.console.log(r.asInstanceOf[js.Any])
                                    cb(
                                      Option(e)
                                        .map(js.JavaScriptException(_))
                                        .toLeft(r.toOption),
                                    )
                                  }
                                  Some(F.delay(db.interrupt()))
                                }
                              }.flatMap {
                                case Some(row) =>
                                  println(row)
                                  row.map { x =>
                                    println(x)
                                  }.toList
                                  ???
                                case None => F.pure((Nil, false))
                              }

                        }

                }

        }
    }
