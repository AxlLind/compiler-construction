package punkt0

abstract class Phase[-F, +T] {
  self =>

  def andThen[G](next: Phase[T, G]): Phase[F, G] = new Phase[F, G] {
    def run(v: F)(ctx: Context): G = {
      val result = self.run(v)(ctx)
      Reporter.terminateIfErrors()
      next.run(result)(ctx)
    }
  }

  def run(v: F)(ctx: Context): T
}
