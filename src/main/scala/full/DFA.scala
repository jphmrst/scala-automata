// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.full
import org.typelevel.paiges.Doc
import org.maraist.fa.traits
import org.maraist.fa.styles.AutomatonStyle

/** Partial implementation of a [[traits.DFA DFA]] storing transition
  * information in `Array`s.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam Z Type of style options for Graphviz export
 *
 * @group DFA
 */
trait DFA[S, T, -Z[S, T] <: AutomatonStyle[S, T]]

extends traits.DFA[S, T, Z] with UnindexedDFA[S, T, Z] with FA[S, T, Z] {

  protected val transitionsMatrix: Array[Array[Int]]

  override def initialStateIndices: Set[Int] = Set(initialStateIndex)

  override val initialState: S = state(initialStateIndex)

  override val initialStates: Set[S] = Set(initialState)

  import scala.util.control.NonLocalReturns.*
  override def accepts(string: Seq[T]): Boolean = returning {
    var current = initialStateIndex
    for(t <- string) {
      val ti = transitionsSeq.indexOf(t)
      current = transitionsMatrix(current)(ti)
      if (current<0) throwReturn(false);
    }
    finalStateIndices.contains(current)
  }

  override def transition(s: S, t: T): Option[S] = {
    val fromIdx: Int = stateSeq.indexOf(s)
    val labelIdx: Int = transitionsSeq.indexOf(t)
    val toIdx: Option[Int] = transitionIndex(fromIdx,labelIdx)
    toIdx.map(stateSeq)
  }

  override def transitionIndex(si: Int, ti: Int): Option[Int] =
    if (si > -1)
      if (ti > -1) {
        val si2 = transitionsMatrix(si)(ti)
        if (si2 > -1) Some(si2) else None
      } else None
      else None

  override protected def prettyHeader: Doc = Doc.text("---------- DFA dump")

  override def map[S2, T2](stateMap: S => S2, transitionMap: T => T2):
      DFA[S2, T2, Z] =
    assembleDFA(
      stateSeq.map(stateMap),
      transitionsSeq.map(transitionMap),
      initialStateIndex, finalStateIndices, transitionsMatrix)

  override def mapStates[S2](stateMap: S => S2): DFA[S2, T, Z] =
    map(stateMap, (t: T) => t)

  override def mapTransitions[T2](transitionMap: T => T2): DFA[S, T2, Z] =
    map((s: S) => s, transitionMap)

  /** Internal method for instantiating a DFA of the appropriate runtime
    * type.
    */
  def assembleDFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initialStateIndex: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]]
  ): DFA[S0, T0, Z]

}
