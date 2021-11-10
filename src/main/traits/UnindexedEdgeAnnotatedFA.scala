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
trait UnindexedEdgeAnnotatedFA[
  S, T, A,
  -Z[ZS, ZT, ZA] <: EdgeAnnotatedAutomatonStyle[ZS, ZT, ZA]]

extends UnindexedFA[S, T, [ZS, ZT] =>> Z[ZS, ZT, A]] {

  /** Return the annotation (if any) on the transition from `src` to
    * `dest` labelled `label`.
    */
  def annotation(src: S, label: T, dest: S): Option[A]

  /** Check whether there is an annotation on the transition from `src`
    * to `dest` labelled `label`.
    */
  def annotated(src: S, label: T, dest: S): Boolean

  /** Return the annotation (if any) on the e-transition from `src` to
    * `dest`.  For automata without e-transitions, this method will
    * always return `None`.
    */
  def eAnnotation(src: S, dest: S): Option[A]

  /** Check whether there is an annotation on the unlabeled transition
    * from `src` to `dest`.  For automata without e-transitions, this
    * method will always return `false`.
    */
  def eAnnotated(src: S, dest: S): Boolean

  /** Perform an action for every annotated labelled edge in this
    * automaton.
    */
  def foreachEdgeAnnotation(action: (S, T, S, A) => Any): Unit

  /** Perform an action for every annotated unlabelled edge in this
    * automaton.
    */
  def foreachEdgeAnnotation(action: (S, S, A) => Any): Unit

//  /** Derive a new isomorphic automaton from this one by applying
//    * transforms to the states, transitions and annotations.  This method
//    * does not check that either map is injective: if they are not, then the
//    * result may be unpredictable.
//    */
//  def map[S2, T2, A2](
//    stateMap: S => S2, transitionMap: T => T2, annMap: A => A2):
//      UnindexedEdgeAnnotatedFA[S2, T2, A2, Z]
//
//  /** Derive a new isomorphic automaton from this one by applying
//    * transforms to the states.  This method does not check that the
//    * `stateMap` is injective: if it is not, then the result may have
//    * separate states which cannot be distinguished.
//    */
//  def mapAnnotations[A2](annotationsMap: A => A2):
//      UnindexedEdgeAnnotatedFA[S, T, A2, Z]
}
