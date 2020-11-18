package punkt0

import java.io.File

case class Context(
  file: Option[File] = None,
  outDir: Option[File] = None,
  doPrintMain: Boolean = false,
  doTokens: Boolean = false,
  doAST: Boolean = false,
  doASTPlus: Boolean = false,
  doSymbolIds: Boolean = false,
  doCBackend: Boolean = false,
)
