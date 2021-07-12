// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.annotated
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.fa.DFA
import org.maraist.fa.DFA.DFAelements
import org.maraist.fa.impl.{AbstractArrayDFA, AbstractHashDFABuilder}

/** Implementation of a [[org.maraist.fa.DFA DFA]] using
 *  [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
 *  `Array`s.
 * @param initialStateIndex Index of the initial state of the automaton.
 * @param finalStateIndices Set of the indices of the final states of the
 * automaton
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 *
 * @group DFA
 */
abstract class AbstractEdgeAnnotatedArrayDFA[S, T, A](
  stateSeq: IndexedSeq[S],
  initialStateIndex: Int,
  finalStateIndices: Set[Int],
  transitionsSeq: IndexedSeq[T],
  transitionsMatrix: Array[Array[Int]],
  val edgeAnnotations: Array[Array[Option[A]]]
)
    extends AbstractArrayDFA[S,T](
      stateSeq, initialStateIndex, finalStateIndices,
      transitionsSeq, transitionsMatrix
    )
    with DFAEdgeAnnotations[S,T,A] {

  /** Return the annotation (if any) on the transition from `src`
    * labelled `label`.
    */
  def annotation(src: S, label: T): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label))

  /** Return the annotation (if any) on the transition from the state at
    * index `srcIdx` with the label with index `labelIdx`.
    */
  def annotationIndex(srcIdx: Int, labelIdx: Int): Option[A] =
    edgeAnnotations(srcIdx)(labelIdx)
}

class EdgeAnnotatedArrayDFA[S,T,A](
  stateSeq: IndexedSeq[S],
  initialStateIndex: Int,
  finalStateIndices: Set[Int],
  transitionsSeq: IndexedSeq[T],
  transitionsMatrix: Array[Array[Int]],
  edgeAnnotations: Array[Array[Option[A]]]
)
extends AbstractEdgeAnnotatedArrayDFA[S,T,A](
  stateSeq, initialStateIndex, finalStateIndices,
  transitionsSeq, transitionsMatrix, edgeAnnotations
) {
  type Traverser = org.maraist.fa.DFA.DFAtraverser[S,T]
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) = ???
}

// -----------------------------------------------------------------
// DFA builder
// -----------------------------------------------------------------

abstract class AbstractHashEdgeAnnotatedDFABuilder
  [S, T, A,
    D <: AbstractEdgeAnnotatedArrayDFA[S,T,A],
    K >: Elements.AnnotatedDFAelement[S,T,A] <: Matchable
  ](initialState: S)
    extends AbstractHashDFABuilder[S,T,D,K](initialState)
    with DFAEdgeAnnotationsBuilder[S,T,A,D,K] {

  val edgeAnnotations: HashMap[S, HashMap[T, A]] =
    new HashMap[S, HashMap[T, A]]

  def annotation(src: S, label: T): Option[A] =
    edgeAnnotations.get(src) match {
      case None => None
      case Some(subhash) => subhash.get(label) match {
        case None => None
        case Some(ann) => Some(ann)
      }
    }

  def setAnnotation(src: S, label: T, annotation: A): Unit = {
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

  def removeAnnotation(src: S, label: T): Unit =
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

  protected final def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]]
  ): D = {

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
  ): D


  /** Helper method for the [[scala.collection.mutable.Builder]]
    * implementation.
    */
  override protected def addBuilderElement(builder: K): Unit =
    builder match {
      case Elements.SetAnnotation(src, _, label, ann) =>
        setAnnotation(src, label, ann)
      case Elements.RemoveAnnotation(src, _, label) =>
        removeAnnotation(src, label)
      case e: DFAelements[S, T] => super.addBuilderElement(builder)
    }
}

class HashEdgeAnnotatedDFABuilder[S, T, A](initialState: S)
extends AbstractHashEdgeAnnotatedDFABuilder[
  S, T, A, EdgeAnnotatedArrayDFA[S, T, A],
  Elements.AnnotatedDFAelement[S,T,A]
](initialState) {

  protected def assembleDFA(
    statesSeq: IndexedSeq[S],
    initialIdx: Int,
    finalStateIndices: HashSet[Int],
    transitionsSeq: IndexedSeq[T],
    idxLabels: Array[Array[Int]],
    edgeAnnotationsArray: Array[Array[Option[A]]]):
      EdgeAnnotatedArrayDFA[S, T, A] =
    new EdgeAnnotatedArrayDFA[S,T,A](
      statesSeq,
      initialIdx,
      Set.from(finalStateIndices),
      transitionsSeq,
      idxLabels,
      edgeAnnotationsArray
    )

  // TODO
  protected def dotTraverser(sb: StringBuilder, stateList: IndexedSeq[S]):
      Traverser = ???
}
