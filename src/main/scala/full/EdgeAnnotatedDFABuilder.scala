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
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.fa.elements.*
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle
import org.maraist.fa.traits

// -----------------------------------------------------------------
// DFA builder
// -----------------------------------------------------------------

/** Partial implementation of a builder for DFAs using
  * [[scala.collection.mutable.HashSet `HashSet`s]] and
  * [[scala.collection.mutable.HashMap `HashMap`s]].
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam D Type of DFA constructed by this builder.
  * @tparam K Builder elements for this builder.
  * @tparam Z Type of style options for Graphviz export
  *
  * @group DFA
  */
trait EdgeAnnotatedDFABuilder[
  S, T, A,
  +D[DS, DT, DA] <: EdgeAnnotatedDFA[DS, DT, DA, Z],
  -K >: EdgeAnnotatedDFAelements[S, T, A] <: Matchable,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]
]

extends traits.EdgeAnnotatedDFABuilder[S, T, A, D, K, Z]

with UnindexedEdgeAnnotatedFA[S, T, A, Z]

with DFABuilder[
  S, T,
  [DS, DT] =>> D[DS, DT, A],
  K,
  [ZS, ZT] =>> Z[ZS, ZT, A]] {

  protected val edgeAnnotations: HashMap[S, HashMap[T, A]] =
    new HashMap[S, HashMap[T, A]]

  override def annotation(src: S, label: T): Option[A] =
    edgeAnnotations.get(src) match {
      case None => None
      case Some(subhash) => subhash.get(label) match {
        case None => None
        case Some(ann) => Some(ann)
      }
    }

  override def setAnnotation(src: S, label: T, annotation: A): Unit = {
    val subhash: HashMap[T, A] = edgeAnnotations.get(src) match {
      case Some(h) => h
      case None => {
        val h = new HashMap[T, A]
        edgeAnnotations(src) = h
        h
      }
    }
    subhash(label) = annotation
  }

  override def removeAnnotation(src: S, label: T, dest: S): Unit =
    transition(src, label) match {
      case None => { }
      case Some(d) =>
        if dest.equals(d) then removeAnnotation(src, label) else { }
    }

  override def setAnnotation(src: S, label: T, dest: S, annotation: A): Unit =
    transition(src, label) match {
      case None => { }
      case Some(d) =>
        if dest.equals(d) then setAnnotation(src, label, annotation) else { }
    }

  override def annotation(src: S, label: T, dest: S): Option[A] =
    transition(src, label) match {
      case None => None
      case Some(d) => if dest.equals(d) then annotation(src, label) else None
    }
  override def annotated(src: S, label: T, dest: S): Boolean =
    annotation(src, label, dest).isDefined

  override def eAnnotated(src: S, dest: S): Boolean = false
  override def eAnnotation(src: S, dest: S): Option[A] = None

  override def removeAnnotation(src: S, label: T): Unit =
    edgeAnnotations.get(src) match {
      case Some(subhash) => {
        subhash.get(label) match {
          case Some(_) => {
            subhash -= label
            if subhash.size == 0
            then edgeAnnotations -= src
          }
          case None => { }
        }
      }
      case None => { }
    }

  /** {@inheritDoc} */
  def initialAnnotation: Option[A] = initialAnn

  protected var initialAnn: Option[A] = None

  /** {@inheritDoc} */
  override def addInitialAnnotation(annotation: A): Unit = {
    initialAnn = Some(annotation)
  }

  /** {@inheritDoc} */
  override def removeInitialAnnotation: Unit = {
    initialAnn = None
  }

  /** {@inheritDoc}
    */
  override def initialAnnotated: Boolean = initialAnn.isDefined

  protected def edgeAnnotatedDfaComponents: (
    IndexedSeq[S], IndexedSeq[T], Int, HashSet[Int], Array[Array[Int]]
  ) = {
    val (statesSeq, transitionsSeq, initialIdx, finalStateIndices, idxLabels) =
      dfaComponents

    (statesSeq, transitionsSeq, initialIdx, finalStateIndices, idxLabels)
  }

  protected def getEdgeAnnotatedArray(
    statesSeq: IndexedSeq[S],
    transitionsSeq: IndexedSeq[T]
  ): Array[Array[Option[A]]] = {
    val edgeAnnotationsArray: Array[Array[Option[A]]] =
      Array.fill[Option[A]](statesSeq.length, transitionsSeq.length)(None)

    for (i <- 0 until statesSeq.length) {
      val src = statesSeq(i)
      val subhash = edgeAnnotations(src)

      for(j <- 0 until transitionsSeq.length) {
        val label = transitionsSeq(j)
        subhash.get(label) match {
          case Some(ann) => edgeAnnotationsArray(i)(j) = Some(ann)
          case None => { }
        }
      }
    }

    edgeAnnotationsArray
  }

  def map[S2, T2, A2](
    stateMap: S => S2, transitionMap: T => T2, annMap: A => A2):
      D[S2, T2, A2] = {
    val (statesSeq, transitionsSeq, initialIdx, finalStateIndices, idxLabels) =
      edgeAnnotatedDfaComponents
    derivedDFA(
      statesSeq.map(stateMap),
      transitionsSeq.map(transitionMap),
      initialIdx,
      finalStateIndices.toSet,
      idxLabels,
      getEdgeAnnotatedArray(statesSeq, transitionsSeq)
        .map(_.map(_.map(annMap))),
      initialAnn.map(annMap)
    )
  }

  override def map[S2, T2](stateMap: S => S2, transitionMap: T => T2):
      D[S2, T2, A] =
    map(stateMap, transitionMap, (a: A) => a)

  def mapAnnotations[A2](annMap: A => A2): D[S, T, A2] =
    map((s: S) => s, (t: T) => t, annMap)

  override def mapStates[S2](stateMap: S => S2): D[S2, T, A] =
    map(stateMap, (t: T) => t, (a: A) => a)

  override def mapTransitions[T2](transitionMap: T => T2): D[S, T2, A] =
    map((s: S) => s, transitionMap, (a: A) => a)

  override protected def derivedDFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initialIdx: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]]
  ): D[S0, T0, A] = derivedDFA(
    stateSeq, transitionsSeq,
    initialIdx, finalStateIndices, transitionsMatrix,
    Array.fill[Option[A]](stateSeq.length, transitionsSeq.length)(None),
    initialAnn
  )

  protected def derivedDFA[S0, T0, A0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    initialStateIndex: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]],
    edgeAnnotations: Array[Array[Option[A0]]],
    initialAnn: Option[A0] = None
  ): D[S0, T0, A0]

  override protected final def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]]
  ): D[S, T, A] = {
    val edgeAnnotationsArray = getEdgeAnnotatedArray(statesSeq, transitionsSeq)

    assembleDFA(
      statesSeq, initialIdx, finalStateIndices, transitionsSeq, idxLabels,
      edgeAnnotationsArray
    )
  }

  protected def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]],
    edgeAnnotationsArray: Array[Array[Option[A]]]
  ): D[S, T, A]


  /** Helper method for the [[scala.collection.mutable.Builder]]
    * implementation.
    */
  override def addOne(builder: K): this.type = {
    builder match {
      case SetAnnotation(src, label, _, ann): SetAnnotation[S, T, A] =>
        setAnnotation(src, label, ann)
      case RemoveAnnotation(src, label, _): RemoveAnnotation[S, T, A] =>
        removeAnnotation(src, label)
      case _ => super.addOne(builder)
    }
    this
  }
}
