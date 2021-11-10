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
import org.maraist.fa.full
import org.maraist.fa.elements
import org.maraist.fa.styles.ProbabilisticAutomatonStyle

/**
  * Concrete builder class for {@link org.maraist.fa.DFA DFAs} based
  * on hash tables.
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on transitions of the automaton
  *
  *  @group PFA
  */
class PFABuilder[S, T]

extends full.PFABuilder[
  S, T, PFA, elements.PFAelements[S, T], ProbabilisticAutomatonStyle] {

  protected def assemblePFA(statesSeq: IndexedSeq[S],
                            initialProbs: Array[Double],
                            finalProbs: Array[Double],
                            transitionsSeq: IndexedSeq[T],
                            transitionsMatrix: Array[Array[Array[Double]]],
                            eTransitionsMatrix: Array[Array[Double]]
                          ): PFA[S, T] =
    new PFA[S, T](
      statesSeq, initialProbs, finalProbs,
      transitionsSeq, transitionsMatrix, eTransitionsMatrix)
}
