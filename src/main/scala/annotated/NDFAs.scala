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
import org.maraist.graphviz.{GraphvizOptions}
import org.maraist.util.IndexSetsTracker
import org.maraist.fa.NDFA
import org.maraist.fa.NDFA.NDFAelements
import org.maraist.fa.impl.
  {AbstractArrayDFA, AbstractArrayNDFA, AbstractHashNDFABuilder}

abstract class AbstractEdgeAnnotatedArrayNDFA
  [S, T, +ThisDFA <: AbstractEdgeAnnotatedArrayDFA[Set[S],T,DA],
    DA, NA]
  (stateSeq: IndexedSeq[S],
    initialStateSet: Set[Int],
    finalStateSet: Set[Int],
    transitionsSeq: IndexedSeq[T],
    transitionsArray: Array[? <: Array[? <: Set[Int]]],
    epsilons: Array[? <: Set[Int]],
    protected val labelledEdgeAnnotations:
        Array[? <: Array[? <: Array[? <: Option[NA]]]],
    protected val unlabelledEdgeAnnotations:
        Array[? <: Array[? <: Option[NA]]])
  (using combiner: EdgeAnnotationCombiner[NA, DA])
extends AbstractArrayNDFA[S, T, ThisDFA]
  (stateSeq, initialStateSet, finalStateSet,
    transitionsSeq, transitionsArray, epsilons)
    with EdgeAnnotatedNDFA[S, T, NA, DA, ThisDFA] {

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, label: T, dest: S): Option[NA] =
    annotationIndex(indexOf(src), labelIndex(label), indexOf(dest))

  /** Return the annotation (if any) on the transition from the state at
    * index `srcIdx` with the label with index `labelIdx`.
    */
  def annotationIndex(srcIdx: Int, labelIdx: Int, destIdx: Int): Option[NA] =
    labelledEdgeAnnotations(srcIdx)(labelIdx)(destIdx)

  /** Return the annotation (if any) on the unlabelled transition from
    * `src` to `dest`.
    */
  def annotation(src: S, dest: S): Option[NA] =
    annotationIndex(indexOf(src), indexOf(dest))

  /** Return the annotation (if any) on the unlabelled transition from
    * the state at index `srcIdx`.
    */
  def annotationIndex(srcIdx: Int, destIdx: Int): Option[NA] =
    unlabelledEdgeAnnotations(srcIdx)(destIdx)

  override protected def dumpTransition(src: S, label: T, dest: S): Unit = {
    print("- " + src + " -[ " + label)
    annotation(src, label, dest) match {
      case None => { print(" (unann.)") }
      case Some(a) => { print(" : " + a) }
    }
    print(" ]-> " + dest)
    println()
  }

  override protected def dumpTransition(src: S, dest: S): Unit = {
    println("- " + src + " -{ ")
    annotation(src, dest) match {
      case None => { print(" (unann.)") }
      case Some(a) => { print(" : " + a) }
    }
    print(" }-> " + dest)
  }

  protected def assembleDFA(
    dfaStates:IndexedSeq[Set[S]],
    initialStateIdx:Int,
    dfaFinals:Set[Int],
    transitionsSeq:IndexedSeq[T],
    dfaTransitions:Array[Array[Int]],
    tracker:IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[DA]]]): ThisDFA

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
    val nfaUnlabelledCombined: Array[Option[DA]] =
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
                val updated: DA =
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
    val dfaUnlabelledCombined: Array[Option[DA]] =
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
            val newCombined: DA =
              combiner.combined(dfaUnlabelledCombined(dfaSrc), thisAnn)

            dfaUnlabelledCombined(dfaSrc) = Some(newCombined)
          }
        }
      }
    }

    // Finally, the third loop is for the labelled transitions in the
    // DFA.
    val edgeAnnotations: Array[Array[Option[DA]]] =
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
                      case None => combiner.single(ann)
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

class EdgeAnnotatedArrayNDFA[S, T, DA, NA](
  stateSeq: IndexedSeq[S],
  initialStateSet: Set[Int],
  finalStateIndices: Set[Int],
  transitionsSeq: IndexedSeq[T],
  transitionsMatrix: Array[Array[Set[Int]]],
  epsilons: Array[? <: Set[Int]],
  labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
  unlabelledEdgeAnnotations: Array[Array[Option[NA]]]
)
  (using combiner: EdgeAnnotationCombiner[NA, DA])
extends AbstractEdgeAnnotatedArrayNDFA
  [S, T, EdgeAnnotatedArrayDFA[Set[S],T,DA], DA, NA]
  (stateSeq, initialStateSet, finalStateIndices,
    transitionsSeq, transitionsMatrix, epsilons,
    labelledEdgeAnnotations, unlabelledEdgeAnnotations
  ) {

  // println("** " + initialStateSet)

  // for(src <- stateSeq)
  //   for(label <- transitionsSeq)
  //     for(dest <- stateSeq)
  //       labelledEdgeAnnotations(
  //         stateSeq.indexOf(src)
  //       )(
  //         transitionsSeq.indexOf(label)
  //       )(
  //         stateSeq.indexOf(dest)
  //       ) match {
  //         case None => {}
  //         case Some(ann) => printf(
  //           "** [%s][%s][%s] = %s\n",
  //           src, label, dest, ann
  //         )
  //       }

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
    edgeAnnotations: Array[Array[Option[DA]]]):
      EdgeAnnotatedArrayDFA[Set[S],T,DA] =
    new EdgeAnnotatedArrayDFA[Set[S],T,DA](
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq,
      dfaTransitions, edgeAnnotations)
}


// -----------------------------------------------------------------
// NDFA builder
// -----------------------------------------------------------------

abstract class AbstractHashEdgeAnnotatedNDFABuilder
  [S, T, DA, NA,
    +D <: AbstractEdgeAnnotatedArrayDFA[Set[S],T,DA],
    +N <: AbstractEdgeAnnotatedArrayNDFA[S,T,D,DA,NA],
    E >: Elements.AnnotatedNDFAelement[S,T,NA] <: Matchable]
(using combiner: EdgeAnnotationCombiner[NA, DA])

    extends AbstractHashNDFABuilder[S,T,D,N,E]
    with NDFAEdgeAnnotationsBuilder[S,T,NA,DA,D,N,E] {

  protected val labelledEdgeAnnotations:
      HashMap[S, HashMap[T, HashMap[S, NA]]] =
    new HashMap[S, HashMap[T, HashMap[S, NA]]]

  def annotation(src: S, label: T, dest: S): Option[NA] =
    labelledEdgeAnnotations.get(src).map(_(label)).map(_(dest))

  def setAnnotation(src: S, label: T, dest: S, annotation: NA): Unit =
    labelledEdgeAnnotations.get(src) match {
      case None => {
        labelledEdgeAnnotations(src) =
          HashMap(label -> HashMap(dest -> annotation))
      }
      case Some(submap1) => submap1.get(label) match {
        case None => { submap1(label) = HashMap(dest -> annotation) }
        case Some(submap2) => { submap2(dest) = annotation }
      }
    }

  def removeAnnotation(src: S, label: T, dest: S): Unit =
    labelledEdgeAnnotations.get(src) match {
      case None => { }
      case Some(submap1) => submap1.get(label) match {
        case None => { }
        case Some(submap2) => {
          submap2 -= dest
          if (submap2.isEmpty) {
            submap1 -= label
            if (submap1.isEmpty) {
              labelledEdgeAnnotations -= src
            }
          }
        }
      }
    }

  protected val unlabelledEdgeAnnotations: HashMap[S, HashMap[S, NA]] =
    new HashMap[S, HashMap[S, NA]]

  def annotation(src: S, dest: S): Option[NA] =
    unlabelledEdgeAnnotations.get(src).map(_(dest))

  def setEAnnotation(src: S, dest: S, annotation: NA): Unit =
    unlabelledEdgeAnnotations.get(src) match {
      case None => {
        unlabelledEdgeAnnotations(src) = HashMap(dest -> annotation)
      }
      case Some(submap) => { submap(dest) = annotation }
    }

  def removeEAnnotation(src: S, dest: S): Unit =
    unlabelledEdgeAnnotations.get(src) match {
      case None => { }
      case Some(submap) => {
        submap -= dest
        if (submap.isEmpty) then unlabelledEdgeAnnotations -= src
      }
    }

  import Elements.*
  override protected def addBuilderElement(elem: E): Unit = elem match {
    case SetAnnotation(src, label, dest, annotation): SetAnnotation[S,T,NA] =>
      setAnnotation(src, label, dest, annotation)
    case RemoveAnnotation(src, label, dest): RemoveAnnotation[S,T,NA] =>
      removeAnnotation(src, label, dest)
    case SetEAnnotation(src, dest, annotation): SetEAnnotation[S, NA] =>
      setEAnnotation(src, dest, annotation)
    case RemoveEAnnotation(src, dest): RemoveEAnnotation[S, NA] =>
      removeEAnnotation(src, dest)
    case e: NDFAelements[S,T] => super.addBuilderElement(e)
  }

  protected override def assembleNDFA(
    statesSeq: IndexedSeq[S], initials: Set[Int], finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[HashSet[Int]]],
    epsilonsArray: Array[HashSet[Int]]):
      N = {
    // println("*** " + initials)
    assembleNDFA(
      statesSeq, initials, finals, transitionsSeq,
      labelsArray.map(_.map(_.toSet)),
      epsilonsArray.map(_.toSet),
      Array.tabulate(statesSeq.size, transitionsSeq.size, statesSeq.size)(
        (s1, t, s2) => labelledEdgeAnnotations.get(statesSeq(s1))
          .flatMap(_.get(transitionsSeq(t)))
          .flatMap(_.get(statesSeq(s2)))),
      Array.tabulate(statesSeq.size, statesSeq.size)(
        (s1, s2) => unlabelledEdgeAnnotations.get(statesSeq(s1))
          .flatMap(_.get(statesSeq(s2)))))
  }

  protected def assembleNDFA(
    statesSeq: IndexedSeq[S], initials: Set[Int], finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
    unlabelledEdgeAnnotations: Array[Array[Option[NA]]]):
      N

  def toDFA: D = toNDFA.toDFA
}

class HashEdgeAnnotatedNDFABuilder[S, T, DA, NA]
  (using combiner: EdgeAnnotationCombiner[NA, DA])
extends AbstractHashEdgeAnnotatedNDFABuilder[
  S, T, DA, NA,
  EdgeAnnotatedArrayDFA[Set[S], T, DA],
  EdgeAnnotatedArrayNDFA[S, T, DA, NA],
  Elements.AnnotatedNDFAelement[S,T,NA]
] {

  protected override def assembleNDFA(
    statesSeq: IndexedSeq[S], initials: Set[Int], finals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
    unlabelledEdgeAnnotations: Array[Array[Option[NA]]]):
      EdgeAnnotatedArrayNDFA[S, T, DA, NA] =
    new EdgeAnnotatedArrayNDFA[S, T, DA, NA](
      statesSeq, initials, finals, transitionsSeq, labelsArray, epsilonsArray,
      labelledEdgeAnnotations, unlabelledEdgeAnnotations
    )

}

