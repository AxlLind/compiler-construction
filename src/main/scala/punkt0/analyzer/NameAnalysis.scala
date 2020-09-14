package punkt0
package analyzer

import ast.Trees._
import Symbols._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

}
