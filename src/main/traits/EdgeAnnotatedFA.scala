// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.traits
import org.maraist.fa.util.EdgeAnnotationCombiner
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle

/** Methods provided by an edge-annotated nondeterministic
  * (non-indexed) finite automata (NFA).
  *
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam A The type of annotations on transitions
  * @tparam Z Style options for Graphviz layout of these automata
  *
  * @group Annotated
  */
trait EdgeAnnotatedFA[S, T, A,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]]

extends FA[S, T, [ZS, ZT] =>> Z[ZS, ZT, A]]

with UnindexedEdgeAnnotatedFA[S, T, A, Z] {

  /** Return the annotation (if any) on the transition from the state at
    * index `srcIdx` to the state at index `destIdx` via the label
    * with index `labelIdx`.
    */
  def annotationIndex(srcIdx: Int, labelIdx: Int, destIdx: Int): Option[A]

  /** Return the annotation (if any) on the unlabelled transition from
    * the state at index `srcIdx` to the state with index `destIdx`.
    */
  def eAnnotationIndex(srcIdx: Int, destIdx: Int): Option[A]

}
