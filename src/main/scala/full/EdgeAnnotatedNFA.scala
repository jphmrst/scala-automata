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
import org.maraist.fa.util.Paiges.toDoc
import org.maraist.fa.util.{EdgeAnnotationCombiner, IndexSetsTracker, Grid}
import org.maraist.fa.styles.EdgeAnnotatedAutomatonStyle
import org.maraist.fa.traits

trait EdgeAnnotatedNFA[
  S, T, NA, DA,
  G[X] <: Set[X],
  D[DS, DT, DDA] <: EdgeAnnotatedDFA[DS, DT, DDA, DZ],
  NZ[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA],
  DZ[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]]

extends NFA[
  S, T, G,
  [DS, DT] =>> D[DS, DT, DA],
  [ZS, ZT] =>> NZ[ZS, ZT, NA],
  [ZS, ZT] =>> DZ[ZS, ZT, DA]]

with traits.EdgeAnnotatedNFA[S, T, NA, DA, G, D, NZ, DZ]

with UnindexedEdgeAnnotatedFA[S, T, NA, NZ] {

  protected def labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]]
  protected def unlabelledEdgeAnnotations: Array[Array[Option[NA]]]
  protected def combiner: EdgeAnnotationCombiner[NA, DA]

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

  def eAnnotation(src: S, dest: S): Option[NA] =
    eAnnotationIndex(indexOf(src), indexOf(dest))

  def eAnnotationIndex(srcIdx: Int, destIdx: Int): Option[NA] =
    unlabelledEdgeAnnotations(srcIdx)(destIdx)

  override def map[S2, T2](stateMap: S => S2, transitionMap: T => T2):
      EdgeAnnotatedNFA[S2, T2, NA, DA, G, D, NZ, DZ] =
    derivedNFA(
      stateSeq.map(stateMap), transitionsSeq.map(transitionMap),
      transitionsArray, epsilonsArray,
      finalStateIndices, initialStateIndices,
      labelledEdgeAnnotations, unlabelledEdgeAnnotations)

  override def mapStates[S2](stateMap: S => S2):
      EdgeAnnotatedNFA[S2, T, NA, DA, G, D, NZ, DZ] =
    map(stateMap, (t: T) => t)

  override def mapTransitions[T2](transitionMap: T => T2):
      EdgeAnnotatedNFA[S, T2, NA, DA, G, D, NZ, DZ] =
    map((s: S) => s, transitionMap)

  override def derivedNFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    transitionsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    finalStateIndices: Set[Int],
    initialStateIndices: Set[Int]):
      EdgeAnnotatedNFA[S0, T0, NA, DA, G, D, NZ, DZ] =
    derivedNFA(
      stateSeq, transitionsSeq,
      transitionsArray, epsilonsArray,
      finalStateIndices, initialStateIndices,
      labelledEdgeAnnotations, unlabelledEdgeAnnotations)

  /** Internal method for instantiating an NFA of the appropriate
    * runtime type.
    */
  def derivedNFA[S0, T0](
    stateSeq: IndexedSeq[S0],
    transitionsSeq: IndexedSeq[T0],
    transitionsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    finalStateIndices: Set[Int],
    initialStateIndices: Set[Int],
    labelledEdgeAnnotations: Array[Array[Array[Option[NA]]]],
    unlabelledEdgeAnnotations: Array[Array[Option[NA]]]):
      EdgeAnnotatedNFA[S0, T0, NA, DA, G, D, NZ, DZ]

  override protected def prettyHeader: Doc =
    Doc.text("---------- EdgeAnnotatedNFA dump")

  protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    initialAnnotation: Option[DA],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[DA]]]): D[G[S], T, DA]

  private final val debugEAassemble: Boolean = false
  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]]):
      D[G[S], T, DA] = {
    val nfaStatesCount = stateSeq.size
    val dfaStatesCount = dfaStates.size
    val labelsCount = transitionsSeq.size

    // All annotations of unlabelled transitions emerging from an NDA
    // state X will be associated with any transition in the DFA *to*
    // any state containing X.  If X is an initial state, then all
    // annotations of unlabelled transitions emerging from an NDA
    // state X will also be included in the inisial annotation of the
    // DFA.  This first loop reviews the unlabelled transitions, and
    // compiles together the annotations assembled for each *source*
    // NFA state, and for the initial DFA annotation.
    val nfaUnlabelledCombined: Array[Option[DA]] =
      Array.fill(nfaStatesCount)(None)
    var initialAnnotation: Option[DA] = None

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

                // Calculate new annotation.
                val updated: DA =
                  combiner.updated(nfaUnlabelledCombined(srcIdx), ann)

                // Write it in.
                nfaUnlabelledCombined(srcIdx) = Some(updated)
              }
            }

            // Now check whether the source edge is an initial state
            // of the NFA.
            if initialStateIndices.contains(srcIdx) then {
              initialAnnotation = Some(initialAnnotation match {
                case None => combiner.single(ann)
                case Some(prevAnn) => combiner.include(prevAnn, ann)
              })
            }
          }
        }
      }
    }

    if (debugEAassemble) {
      println("nfaUnlabelledCombined")
      for (i <- 0 until nfaUnlabelledCombined.size) {
        println(s"${Console.MAGENTA}$i.${Console.BLACK} ${nfaUnlabelledCombined(i).toDoc.render(75)}")
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

    if (debugEAassemble) {
      println(" - - - - - -")
      println("dfaUnlabelledCombined")
      for (i <- 0 until dfaUnlabelledCombined.size) {
        println(s"${Console.MAGENTA}$i.${Console.BLACK} ${dfaUnlabelledCombined(i).toDoc.render(75)}")
      }
      println(" - - - - - -")
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

            // Initialize this annotation with the contents of
            // dfaUnlabelledCombined for the destination DFA node
            // index.
            writingForDfaSrc(labelIdx) = dfaUnlabelledCombined(dfaDestIdx)

            // Iterate through the NFA state indices in this DFA
            // source state.
            for (dest <- dfaDest) {
              val destIdx = stateSeq.indexOf(dest)

              if (debugEAassemble) {
                print(s"${Console.MAGENTA}$srcIdx (in $srcDfaIdx) -[$labelIdx]-> $destIdx (in $dfaDestIdx)${Console.BLACK}  ")
              }

              // Is there an annotation on this NFA edge?
              labelledEdgeAnnotations(srcIdx)(labelIdx)(destIdx) match {
                case None => {
                  if (debugEAassemble) {
                    println(s"${Console.BLUE}no annotation${Console.BLACK}")
                  }
                }
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
                  if (debugEAassemble) {
                    println(s"")
                  }
                }
              }
            }
          }
        }
      }
    }

    if (debugEAassemble) {
      println(" - - - - - -")
      println("edgeAnnotations")
      Grid.gridFormat(edgeAnnotations)
    }

    assembleDFA(
      dfaStates, initialStateIdx, dfaFinals, initialAnnotation, transitionsSeq,
      dfaTransitions, tracker, appearsIn, edgeAnnotations
    )
  }
}
