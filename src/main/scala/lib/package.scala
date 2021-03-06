import spinal.core._
import spinal.lib._

package object lib {
  object MuxCase {
    def apply[T <: Data] (default: T, mapping: Seq[(Bool, T)]): T = {
      var res = default
      for ((t, v) <- mapping.reverse) {
        res = Mux(t, v, res)
      }
      res
    }
  }
}
