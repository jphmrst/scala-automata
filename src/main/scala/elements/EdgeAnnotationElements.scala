// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.elements
import scala.collection.mutable.{Builder}
import org.maraist.fa.traits

case class SetAnnotation[S, T, A](src: S, label: T, dest: S, annotation: A)
case class RemoveAnnotation[S, T, A](src: S, label: T, dest: S)

type LabelledEdgeAnnotationElements[S, T, A] =
  SetAnnotation[S, T, A] | RemoveAnnotation[S, T, A]

type EdgeAnnotatedFAelements[S, T, A] =
  FAelements[S, T] | LabelledEdgeAnnotationElements[S, T, A]

type EdgeAnnotatedDFAelements[S, T, A] =
  DFAelements[S, T] | LabelledEdgeAnnotationElements[S, T, A]

case class SetEAnnotation[S, A](src: S, dest: S, annotation: A)
case class RemoveEAnnotation[S, A](src: S, dest: S)

type UnlabelledEdgeAnnotationElements[S, A] =
  SetEAnnotation[S, A] | RemoveEAnnotation[S, A]

type EdgeAnnotatedNFAelements[S, T, A] = (
  NFAelements[S, T] | LabelledEdgeAnnotationElements[S, T, A]
    | UnlabelledEdgeAnnotationElements[S, A]
)
