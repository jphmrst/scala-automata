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
import scala.collection.mutable.{Builder, HashMap}
import org.maraist.fa.styles.ProbabilisticAutomatonStyle
import org.maraist.fa.full

/** Implementation of a [[org.maraist.fa.DFA DFA]] using
 *  [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
 *  `Array`s.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group PFA
 */
class PFA[S, T](
  protected val stateSeq: IndexedSeq[S],
  val initialProbs: Array[Double],
  val finalProbs: Array[Double],
  protected val transitionsSeq: IndexedSeq[T],
  protected val transitionsMatrix: Array[Array[Array[Double]]],
  protected val eTransitionsMatrix: Array[Array[Double]]
)

extends full.PFA[S, T, ProbabilisticAutomatonStyle] {

  // TODO MAP

}

object PFA {

  def newBuilder[S, T] = new PFABuilder[S, T]

}
