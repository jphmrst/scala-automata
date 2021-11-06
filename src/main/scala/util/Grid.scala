
// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.util

/** FOrmatting arrays as a grid. */
object Grid {

  /** Formatter intended sparse double arrays of [[Option]] values.
    * "Sparse" means that most values are expected to be [[None]].
    * There is no problem per se with using this method on non-sparse
    * arrays, but the result may be more cluttered than you prefer.
    */
  def gridFormat[A](arr: Array[Array[Option[A]]]): Unit = {
    val width = {
      var max = 0
      for (i <- 0 until arr.size) do {
        val row = arr(i).size
        if row > max then max = row
      }
      max
    }

    print(s"${Console.MAGENTA}   ")
    for (j <- 0 until width) do print(f"   $j%3d ")
    println(Console.BLACK)

    for (i <- 0 until arr.size) do {
      val row = arr(i)
      print(f"${Console.MAGENTA}%s$i%3d${Console.BLACK}%s |")
      for (j <- 0 until width)
        do if j < row.size then row(j) match {
          case None => {
            print(" None |")
          }
          case Some(a) => {
            val str = a.toString()
            print(s" $str")
            if str.length > 4 then {
              println()
              print("    |")
              for (k <- 0 to j) do print("      |")
            } else {
              print(str)
              for (k <- str.length until 3) do print(" ")
              print(" |")
            }
          }
        } else if j == width - 1 then {
          print("      |")
        } else {
          print("       ")
        }
      println()
    }
  }
}
