package porcupine

import scala.scalanative.unsafe.*

@extern
private object sqlite3:
  type sqlite3 = CStruct0
  type sqlite3_stmt = CStruct0
  type sqlite3_int64 = CLong
  type sqlite3_uint64 = CLong

  final val SQLITE_OK = 0
  final val SQLITE_ROW = 100
  final val SQLITE_DONE = 101

  final val SQLITE_INTEGER = 1
  final val SQLITE_FLOAT = 2
  final val SQLITE_TEXT = 3
  final val SQLITE_BLOB = 4
  final val SQLITE_NULL = 5

  inline def SQLITE_STATIC: Ptr[Byte] = null

  final val SQLITE_OPEN_READWRITE = 0x00000002
  final val SQLITE_OPEN_CREATE = 0x00000004
  final val SQLITE_OPEN_NOMUTEX = 0x00008000

  def sqlite3_errstr(code: CInt): Ptr[CChar] = extern

  def sqlite3_errmsg(db: Ptr[sqlite3]): Ptr[CChar] = extern

  def sqlite3_open_v2(
      filename: Ptr[CChar],
      ppDb: Ptr[Ptr[sqlite3]],
      flags: CInt,
      zVfs: Ptr[CChar],
  ): CInt = extern

  def sqlite3_close(db: Ptr[sqlite3]): CInt = extern

  def sqlite3_prepare_v2(
      db: Ptr[sqlite3],
      zSql: Ptr[CChar],
      nByte: CInt,
      ppStmt: Ptr[Ptr[sqlite3_stmt]],
      pzTail: Ptr[Ptr[CChar]],
  ): CInt = extern

  def sqlite3_finalize(pStmt: Ptr[sqlite3_stmt]): CInt = extern

  def sqlite3_bind_blob64(
      stmt: Ptr[sqlite3_stmt],
      index: CInt,
      value: Ptr[Byte],
      length: sqlite3_uint64,
      lifetime: Ptr[Byte],
  ): CInt = extern

  def sqlite3_bind_double(
      stmt: Ptr[sqlite3_stmt],
      index: CInt,
      value: CDouble,
  ): CInt = extern

  def sqlite3_bind_int64(
      stmt: Ptr[sqlite3_stmt],
      index: CInt,
      value: sqlite3_int64,
  ): CInt = extern

  def sqlite3_bind_null(
      stmt: Ptr[sqlite3_stmt],
      index: CInt,
  ): CInt = extern

  def sqlite3_bind_text(
      stmt: Ptr[sqlite3_stmt],
      index: CInt,
      value: Ptr[CChar],
      length: CInt,
      lifetime: Ptr[Byte],
  ): CInt = extern

  def sqlite3_step(stmt: Ptr[sqlite3_stmt]): CInt = extern

  def sqlite3_column_blob(stmt: Ptr[sqlite3_stmt], iCol: CInt): Ptr[Byte] = extern

  def sqlite3_column_double(stmt: Ptr[sqlite3_stmt], iCol: CInt): CDouble = extern

  def sqlite3_column_int64(stmt: Ptr[sqlite3_stmt], iCol: CInt): sqlite3_int64 = extern

  def sqlite3_column_text(stmt: Ptr[sqlite3_stmt], iCol: CInt): Ptr[CChar] = extern

  def sqlite3_column_bytes(stmt: Ptr[sqlite3_stmt], iCol: CInt): CInt = extern

  def sqlite3_column_type(stmt: Ptr[sqlite3_stmt], iCol: CInt): CInt = extern

  def sqlite3_column_count(stmt: Ptr[sqlite3_stmt]): CInt = extern

  def sqlite3_interrupt(db: Ptr[sqlite3]): Unit = extern
