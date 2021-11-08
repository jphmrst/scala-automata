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
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.fa.elements.*
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle
import org.maraist.fa.traits

// -----------------------------------------------------------------
// DFA builder
// -----------------------------------------------------------------

/** Instantiable builder for edge-annotated DFAs using
  * [[scala.collection.mutable.HashSet `HashSet`s]] and
  * [[scala.collection.mutable.HashMap `HashMap`s]].
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  */
class EdgeAnnotatedDFABuilder[S, T, A](initialState: S)

extends full.DFABuilder[
  S, T,
  [PS, PT] =>> EdgeAnnotatedDFA[PS, PT, A],
  EdgeAnnotatedDFAelements[S, T, A],
  [ZS, ZT] =>> EdgeAnnotatedAutomatonStyle[ZS, ZT, A]
](initialState)

with full.EdgeAnnotatedDFABuilder[
  S, T, A, EdgeAnnotatedDFA, EdgeAnnotatedDFAelements[S, T, A], EdgeAnnotatedAutomatonStyle
] {

  // TODO MAP

  override protected def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]],
    edgeAnnotationsArray: Array[Array[Option[A]]]):
      EdgeAnnotatedDFA[S, T, A] =
    new EdgeAnnotatedDFA[S, T, A](
      statesSeq,
      initialIdx,
      Set.from(finalStateIndices),
      initialAnn,
      transitionsSeq,
      idxLabels,
      edgeAnnotationsArray
    )
}

