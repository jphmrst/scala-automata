// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import scala.collection.mutable.{Builder, HashMap, HashSet}
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.full

/** Implementation of DFAs.
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group DFA
 */
class DFA[S, T](
  protected val stateSeq: IndexedSeq[S],
  val initialStateIndex: Int,
  val finalStateIndices: Set[Int],
  protected val transitionsSeq: IndexedSeq[T],
  protected val transitionsMatrix: Array[Array[Int]]
)

extends full.DFA[S, T, AutomatonStyle] {
  checkState

  def assembleDFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initialStateIndex: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]]
  ): DFA[S0, T0] = new DFA(
    stateSeq, initialStateIndex,
    finalStateIndices, transitionsSeq, transitionsMatrix)
}

/**
  * @group DFA
  *
  * @groupname DFA Forms of DFAs
  * @groupdesc DFA
  * @groupprio DFA 100
  *
  * @groupname builderElements Arguments to
  * [[scala.collection.mutable.Builder Builder]]s for [[DFA]]s
  * @groupdesc builderElements
  * @groupprio builderElements 150
  *
  * @groupname builderPattern [[scala.collection.mutable.Builder Builder]]
  * pattern method
  * @groupdesc builderPattern
  * @groupprio builderPattern 160
  */
object DFA {
  /** [[scala.collection.mutable.Builder Builder]] pattern method for
    * [[DFA]]s.
    *
    *  @tparam S The type of all states of the automaton
    *  @tparam T The type of labels on transitions of the automaton
    *
    * @group builderPattern
    */
  def newBuilder[S, T](initialState: S): DFABuilder[S, T] =
    new DFABuilder[S, T](initialState)
}
