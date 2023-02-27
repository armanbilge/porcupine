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

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all.*
import scodec.bits.ByteVector

object PorcupineTest extends IOApp.Simple:

  def run = Database.open[IO](":memory:").use { db =>
    db.prepare(Query("create table porcupine (n, i, r, t, b);", Codec.unit, Codec.unit)).use {
      _.cursor(()).use_
    } *> db
      .prepare(
        Query(
          "insert into porcupine values($1, $2, $3, $4, $5);",
          (Codec.`null`, Codec.integer, Codec.real, Codec.text, Codec.blob).tupled,
          Codec.unit,
        ),
      )
      .use(_.cursor((None, 42, 3.14, "lipsum", ByteVector(0, 1, 2, 3))).use_)
  }
