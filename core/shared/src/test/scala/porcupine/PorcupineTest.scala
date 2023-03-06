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

import Codec.*

object PorcupineTest extends IOApp.Simple:

  def run = Database.open[IO](":memory:").use { db =>
    db.execute(sql"create table porcupine (n, i, r, t, b);".command) *>
      db.execute(
        sql"insert into porcupine values(${`null`}, $integer, $real, $text, $blob);".command,
        (None, 42L, 3.14, "quill-pig", ByteVector(0, 1, 2, 3)),
      ) *>
      db.execute(
        sql"select b, t, r, i, n from porcupine;"
          .query(blob *: text *: real *: integer *: `null` *: nil),
      ).flatMap {
        case List((ByteVector(0, 1, 2, 3), "quill-pig", 3.14, 42, None)) => IO.unit
        case other => IO.raiseError(new AssertionError(other))
      }
  }
