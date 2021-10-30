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
import org.maraist.fa.util.EdgeAnnotationCombiner
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle
import org.maraist.fa.traits

/** Implementation of a edge-annotated DFA.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
trait EdgeAnnotatedDFA[S, T, A, Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]]

extends traits.EdgeAnnotatedDFA[S, T, A, Z]

with DFA[S, T, [ZS, ZT] =>> Z[ZS, ZT, A]]

with UnindexedEdgeAnnotatedFA[S, T, A, Z] {

  protected def edgeAnnotations: Array[Array[Option[A]]]

  override def annotation(src: S, label: T): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label))

  override def annotationIndex(srcIdx: Int, labelIdx: Int): Option[A] =
    edgeAnnotations(srcIdx)(labelIdx)

  override def annotation(src: S, label: T, dest: S): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label), indexOf(dest))

  override def annotationIndex(srcIdx: Int, labelIdx: Int, destIdx: Int):
      Option[A] =
    transitionIndex(srcIdx, labelIdx).flatMap(
      ((d: Int) =>
        if d == destIdx then annotationIndex(srcIdx, labelIdx) else None))

  override def eAnnotation(src: S, dest: S): Option[A] = None

  override def eAnnotationIndex(srcIdx: Int, destIdx: Int): Option[A] = None

  /** {@inheritDoc}
    */
  override def initialAnnotated: Boolean = initialAnnotation.isDefined
}
