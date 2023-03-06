# porcupine

[Skunk]-inspired library for [SQLite] on JVM, Node.js, and Native!

```scala
libraryDependencies += "com.armanbilge" %%% "porcupine" % "<version>"
```

[Skunk]: https://github.com/tpolecat/skunk
[SQLite]: https://www.sqlite.org

## Installation

### JVM

Delegates to the [JDBC Driver].

[JDBC Driver]: https://github.com/xerial/sqlite-jdbc

### Node.js

Requires the [better-sqlite3] npm package.

```
npm i better-sqlite3
```

[better-sqlite3]: https://www.npmjs.com/package/better-sqlite3

### Native

You must provide a native build of SQLite3. Here are three interesting ways to do this.

1. Dynamically linking to an existing installation of sqlite3.
```scala
nativeConfig ~= { c => c.withLinkingOptions(c.linkingOptions :+ "-lsqlite3") }
```

2. Statically linking a pre-compiled SQLite into your binary. For example:
```scala
nativeConfig ~= { c => c.withLinkingOptions(c.linkingOptions :+ "/usr/local/Cellar/sqlite/3.41.0/lib/libsqlite3.a") }
```

3. Compiling SQLite as part of your project. You can download the SQLite [amalgation] as a single `sqlite3.c` file and place it in your `resources/scala-native` directory.

[amalgation]: https://www.sqlite.org/amalgamation.html
