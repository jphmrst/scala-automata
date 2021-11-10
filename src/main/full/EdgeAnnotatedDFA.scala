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

  /** Internal storage for the annotations on labelled edges. */
  protected def edgeAnnotations: Array[Array[Option[A]]]

  /** {@inheritDoc} */
  override def annotation(src: S, label: T): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label))

  /** {@inheritDoc} */
  override def annotationIndex(srcIdx: Int, labelIdx: Int): Option[A] =
    edgeAnnotations(srcIdx)(labelIdx)

  /** {@inheritDoc} */
  override def annotation(src: S, label: T, dest: S): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label), indexOf(dest))

  /** {@inheritDoc} */
  override def annotationIndex(srcIdx: Int, labelIdx: Int, destIdx: Int):
      Option[A] =
    transitionIndex(srcIdx, labelIdx).flatMap(
      ((d: Int) =>
        if d == destIdx then annotationIndex(srcIdx, labelIdx) else None))

  /** {@inheritDoc} */
  override def eAnnotation(src: S, dest: S): Option[A] = None

  /** {@inheritDoc} */
  override def eAnnotationIndex(srcIdx: Int, destIdx: Int): Option[A] = None

  /** {@inheritDoc} */
  override def initialAnnotated: Boolean = initialAnnotation.isDefined

  /** {@inheritDoc} Overridden in the
    * [[org.maraist.fa.full.EdgeAnnotatedDFA]] for showing initial
    * annotations.
    */
  override protected
  def plotInitialStateMarker(
    sb: StringBuilder, style: Z[S, T, A], s: S, idx: Int):
      Unit = {

    // Dummy state for arrow base.
    sb ++= "\tinit"
    sb ++= idx.toString
    sb ++= " [shape=none, margin=0, label=\"\"];\n"

    // Arrow from the dummy state to the initial state.
    sb ++= "\tinit"
    sb ++= idx.toString
    sb ++= " -> V"
    sb ++= idx.toString

    // Render any initial annotation.
    initialAnnotation match {
      case None => { }
      case Some(ann) => {
        sb ++= " [label=<"
        sb ++= style.initialAnnotationLabel(ann, s)
        sb ++= ">]"
      }
    }

    sb ++= ";\n"
  }

  override protected def prettyHeader: Doc =
    Doc.text("---------- EdgeAnnotatedDFA dump")

  override protected def prettyTransitions: Doc =
    super.prettyTransitions + (
      Doc.text("- ") + (initialAnnotation match {
        case None => Doc.text("No initial annotation")
        case Some(ann) =>
          Doc.text(s"${Console.MAGENTA}Initial annotation${Console.BLACK} ")
          + Doc.str(ann)
      })).grouped

  def map[S2, T2, A2](
    stateMap: S => S2, transitionMap: T => T2, annMap: A => A2):
      EdgeAnnotatedDFA[S2, T2, A2, Z] =
    assembleDFA(
      stateSeq.map(stateMap),
      transitionsSeq.map(transitionMap),
      initialStateIndex,
      finalStateIndices,
      transitionsMatrix,
      initialAnnotation.map(annMap),
      edgeAnnotations.map(_.map(_.map(annMap)))
    )

  def mapAnnotations[A2](annMap: A => A2): EdgeAnnotatedDFA[S, T, A2, Z] =
    map((s: S) => s, (t: T) => t, annMap)

  override def assembleDFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initialStateIndex: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]]
  ): EdgeAnnotatedDFA[S0, T0, A, Z] = assembleDFA(
    stateSeq, transitionsSeq, initialStateIndex,
    finalStateIndices, transitionsMatrix, initialAnnotation, edgeAnnotations)

  def assembleDFA[S0, T0, A0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initialStateIndex: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]],
    initialAnnotation: Option[A0],
    edgeAnnotations: Array[Array[Option[A0]]]
  ): EdgeAnnotatedDFA[S0, T0, A0, Z]
}
