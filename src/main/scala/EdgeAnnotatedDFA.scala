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
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

/** Implementation of a edge-annotated DFA based on arrays and indexed
  * sequences.
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the
  * automaton
  * @tparam A The type of annotations on the edge of the automaton
  *
  * @group DFA
  */
class EdgeAnnotatedDFA[S, T, A](
  protected val stateSeq: IndexedSeq[S],
  val initialStateIndex: Int,
  val finalStateIndices: Set[Int],
  val initialAnnotation: Option[A],
  protected val transitionsSeq: IndexedSeq[T],
  protected val transitionsMatrix: Array[Array[Int]],
  protected val edgeAnnotations: Array[Array[Option[A]]]
)

extends full.EdgeAnnotatedDFA[S, T, A, EdgeAnnotatedAutomatonStyle] {

  override def assembleDFA[S0, T0, A0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initialStateIndex: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]],
    initialAnnotation: Option[A0],
    edgeAnnotations: Array[Array[Option[A0]]]
  ): EdgeAnnotatedDFA[S0, T0, A0] = new EdgeAnnotatedDFA[S0, T0, A0](
    stateSeq, initialStateIndex, finalStateIndices, initialAnnotation,
    transitionsSeq, transitionsMatrix, edgeAnnotations)

}

object EdgeAnnotatedDFA {
  def newBuilder[S, T, A](initialState: S) =
    new EdgeAnnotatedDFABuilder[S, T, A](initialState)
}
