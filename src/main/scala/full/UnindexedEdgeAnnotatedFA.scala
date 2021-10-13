// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.full
import org.maraist.fa.util.EdgeAnnotationCombiner
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.traits

/** Methods provided by an edge-annotated nondeterministic finite
  * automata (NFA).
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam A The type of annotations on transitions
  * @tparam K Type function/contructor producing the type of
  * annotation on DFAs from the type of annotation on NFAs.
  * @tparam D Type of DFA converted from this NFA
  *
  * @group Annotated
  */
trait UnindexedEdgeAnnotatedFA[S, T, A, Z[X, Y] <: AutomatonStyle[X, Y]]

extends traits.UnindexedEdgeAnnotatedFA[S, T, A, Z]

with UnindexedFA[S, T, Z] {

  override def annotated(src: S, dest: S, label: T): Boolean =
    annotation(src, label, dest).isDefined

  override def annotated(src: S, dest: S): Boolean =
    annotation(src, dest).isDefined

  // =================================================================

  override protected def dumpTransition(src: S, label: T, dest: S): Unit = {
    print("- " + src + " -[ " + label)
    annotation(src, label, dest) match {
      case None => { print(" (unann.)") }
      case Some(a) => { print(" : " + a) }
    }
    print(" ]-> " + dest)
    println()
  }

  override protected def dumpTransition(src: S, dest: S): Unit = {
    println("- " + src + " -{ ")
    annotation(src, dest) match {
      case None => { print(" (unann.)") }
      case Some(a) => { print(" : " + a) }
    }
    print(" }-> " + dest)
  }
}
