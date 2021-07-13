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
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{HashMap, HashSet}
import org.maraist.util.IndexSetsTracker
import org.maraist.fa.NDFA
import org.maraist.fa.NDFA.NDFAelements
import org.maraist.fa.impl.
  {AbstractArrayDFA, AbstractArrayNDFA, AbstractHashNDFABuilder}

abstract class AbstractEdgeAnnotatedArrayNDFA
  [S, T, +ThisDFA <: AbstractEdgeAnnotatedArrayDFA[Set[S],T,K[A]],
    K[_], A <: Matchable]
  (stateSeq: IndexedSeq[S],
    initialStateSet: Set[Int],
    finalStateSet: Set[Int],
    transitionsSeq: IndexedSeq[T],
    transitionsArray: Array[? <: Array[? <: Set[Int]]],
    epsilons: Array[? <: Set[Int]],
    protected val labelledEdgeAnnotations:
        Array[? <: Array[? <: Array[? <: Option[A]]]],
    protected val unlabelledEdgeAnnotations:
        Array[? <: Array[? <: Option[A]]])
  (using combiner: EdgeAnnotationCombiner[A, K])
extends AbstractArrayNDFA[S, T, ThisDFA]
  (stateSeq, initialStateSet, finalStateSet,
    transitionsSeq, transitionsArray, epsilons) {

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, label: T, dest: S): Option[A] =
    annotationIndex(indexOf(src), labelIndex(label), indexOf(src))

  /** Return the annotation (if any) on the transition from the state at
    * index `srcIdx` with the label with index `labelIdx`.
    */
  def annotationIndex(srcIdx: Int, labelIdx: Int, destIdx: Int): Option[A] =
    labelledEdgeAnnotations(srcIdx)(labelIdx)(destIdx)

  /** Return the annotation (if any) on the unlabelled transition from
    * `src` to `dest`.
    */
  def annotation(src: S, dest: S): Option[A] =
    annotationIndex(indexOf(src), indexOf(src))

  /** Return the annotation (if any) on the unlabelled transition from
    * the state at index `srcIdx`.
    */
  def annotationIndex(srcIdx: Int, destIdx: Int): Option[A] =
    unlabelledEdgeAnnotations(srcIdx)(destIdx)

  protected def assembleDFA(
    dfaStates:IndexedSeq[Set[S]],
    initialStateIdx:Int,
    dfaFinals:Set[Int],
    transitionsSeq:IndexedSeq[T],
    dfaTransitions:Array[Array[Int]],
    tracker:IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[K[A]]]]): ThisDFA

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]]):
      ThisDFA = {
    val nfaStatesCount = stateSeq.size
    val dfaStatesCount = dfaStates.size
    val labelsCount = transitionsSeq.size

    // All annotations of unlabelled transitions emerging from an NDA
    // state X will be associated with any transition in the DFA *to*
    // any state containing X.  This first loop reviews the unlabelled
    // transitions, and compiles together the annotations assembled
    // for each *source* NFA state.
    val nfaUnlabelledCombined: Array[Option[K[A]]] =
      Array.fill(nfaStatesCount)(None)

    // Iterate through the NFA state indices as source state.
    for (srcIdx <- 0 until nfaStatesCount) {
      val src = stateSeq(srcIdx)
      val unlabelledForSrc = unlabelledEdgeAnnotations(srcIdx)

      // Iterate through the NFA state indices as destination state.
      for (destIdx <- 0 until nfaStatesCount) {
        val dest = stateSeq(destIdx)

        // Find any annotation on the unlabelled transition from the
        // source to the destination state.
        unlabelledForSrc(destIdx) match {
          case None => { }

          // If there is an annotation on the unlabelled transition,
          case Some(ann) => {

            // Iterate through the DFA state indices corresponding to
            // the source NFA state.
            for (dfaSrc <- appearsIn(srcIdx)) {

              // Iterate through the DFA state indices corresponding to
              // the destination NFA state.
              for (dfaDest <- appearsIn(destIdx)) {

                // Calculate new annotation
                val updated: K[A] =
                  combiner.updated(nfaUnlabelledCombined(srcIdx), ann)

                // Write it in
                nfaUnlabelledCombined(srcIdx) = Some(updated)
              }
            }
          }
        }
      }
    }

    // This second loop calculates the annotation from unlabelled
    // transitions for each state of the DFA, by combining the
    // annotations for the related NFA states.
    val dfaUnlabelledCombined: Array[Option[K[A]]] =
      Array.fill(dfaStatesCount)(None)

    // Iterate through the NFA state indices.
    for (srcIdx <- 0 until nfaStatesCount) {
      val src = stateSeq(srcIdx)
      val thisOpt = nfaUnlabelledCombined(srcIdx)

      // If this state has an annotation
      thisOpt match {
        case None => { }
        case Some(thisAnn) => {

          // Iterate through the DFA state indices corresponding to
          // the source NFA state.
          for (dfaSrc <- appearsIn(srcIdx)) {
            val newCombined: K[A] =
              combiner.combined(dfaUnlabelledCombined(dfaSrc), thisAnn)

            dfaUnlabelledCombined(dfaSrc) = Some(newCombined)
          }
        }
      }
    }

    // Finally, the third loop is for the labelled transitions in the
    // DFA.
    val edgeAnnotations: Array[Array[Option[K[A]]]] =
      Array.fill(dfaStatesCount, labelsCount)(None)

    // Iterate through the DFA state indices.
    for (srcDfaIdx <- 0 until dfaStatesCount) {
      val dfaSrc: Set[S] = dfaStates(srcDfaIdx)
      val writingForDfaSrc = edgeAnnotations(srcDfaIdx)

      // Iterate through the NFA state indices in this DFA source
      // state.
      for (src <- dfaSrc) {
        val srcIdx = stateSeq.indexOf(src)

        // Iterate through the labels.
        for (labelIdx <- 0 until transitionsSeq.length) {
          val label = transitionsSeq(labelIdx)
          val dfaDestIdx = dfaTransitions(srcDfaIdx)(labelIdx)

          // Is there actually a transition here?
          if (dfaDestIdx > -1) {
            val dfaDest: Set[S] = dfaStates(dfaDestIdx)

            // Iterate through the NFA state indices in this DFA
            // source state.
            for (dest <- dfaDest) {
              val destIdx = stateSeq.indexOf(dest)

              // Is there an annotation on this NFA edge?
              labelledEdgeAnnotations(srcIdx)(labelIdx)(destIdx) match {
                case None => { }
                case Some(ann) => {

                  // Find the new value to record against this DFA
                  // edge.
                  val newAnn = writingForDfaSrc(labelIdx) match {

                    // If we have already recorded an annotation for
                    // this DFA edge, combine in the new one.
                    case Some(prev) => combiner.include(prev, ann)

                    // If we have an annotation for this DFA state
                    // from unlabelled transitions, then combine the
                    // present annotation with it instead.
                    case None => dfaUnlabelledCombined(srcDfaIdx) match {
                      case Some(fromUnlab) =>
                        combiner.include(fromUnlab, ann)

                        // Otherwise the new value is from the present
                        // annotation alone.
                        combiner.single(ann)
                    }
                  }

                  // Record the new value
                  writingForDfaSrc(labelIdx) = Some(newAnn)
                }
              }
            }
          }
        }
      }
    }

    assembleDFA(
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq,
      dfaTransitions, tracker, appearsIn, edgeAnnotations
    )
  }
}

class EdgeAnnotatedArrayNDFA[S, T, K[_], A <: Matchable](
  stateSeq: IndexedSeq[S],
  initialStateSet: Set[Int],
  finalStateIndices: Set[Int],
  transitionsSeq: IndexedSeq[T],
  transitionsMatrix: Array[Array[Set[Int]]],
  epsilons: Array[Set[Int]],
  labelledEdgeAnnotations: Array[Array[Array[Option[A]]]],
  unlabelledEdgeAnnotations: Array[Array[Option[A]]]
)
  (using combiner: EdgeAnnotationCombiner[A, K])
extends AbstractEdgeAnnotatedArrayNDFA
  [S, T, EdgeAnnotatedArrayDFA[Set[S],T,K[A]], K, A]
  (stateSeq, initialStateSet, finalStateIndices,
    transitionsSeq, transitionsMatrix, epsilons,
    labelledEdgeAnnotations, unlabelledEdgeAnnotations
  ) {

  // TODO
  protected def dotTraverser(sb:StringBuilder,stateList:IndexedSeq[S]) = ???

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[K[A]]]]):
      EdgeAnnotatedArrayDFA[Set[S],T,K[A]] =
    new EdgeAnnotatedArrayDFA[Set[S],T,K[A]](
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq,
      dfaTransitions, edgeAnnotations)
}
