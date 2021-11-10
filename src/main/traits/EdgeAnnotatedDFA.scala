// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.traits
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

/** Implementation of a edge-annotated DFA.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
trait EdgeAnnotatedDFA[S, T, A, -Z[X, Y, S] <: EdgeAnnotatedAutomatonStyle[X, Y, S]]

extends EdgeAnnotatedFA[S, T, A, Z]

with UnindexedEdgeAnnotatedDFA[S, T, A, Z]

with DFA[S, T, [ZS, ZT] =>> Z[ZS, ZT, A]] {

  /** Return the annotation (if any) on the transition from the state at
    * index `srcIdx` with the label with index `labelIdx`.
    */
  def annotationIndex(srcIdx: Int, labelIdx: Int): Option[A]
}
