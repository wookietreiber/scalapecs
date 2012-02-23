/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                           *
 *  Copyright 2012 Christian Krause. All rights reserved.                    *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                           *
 *  Redistribution and use in source and binary forms, with or without       *
 *  modification, are permitted provided that the following conditions       *
 *  are met:                                                                 *
 *                                                                           *
 *    1. Redistributions of source code must retain the above copyright      *
 *       notice, this list of conditions and the following disclaimer.       *
 *                                                                           *
 *    2. Redistributions in binary form must reproduce the above             *
 *       copyright notice, this list of conditions and the following         *
 *       disclaimer in the documentation and/or other materials              *
 *       provided with the distribution.                                     *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                           *
 *  THIS SOFTWARE IS PROVIDED BY CHRISTIAN KRAUSE ''AS IS'' AND ANY          *
 *  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE        *
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR       *
 *  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL CHRISTIAN KRAUSE OR            *
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,    *
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,      *
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR       *
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   *
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     *
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       *
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                           *
 *  The views and conclusions contained in the software and documentation    *
 *  are those of the authors and should not be interpreted as representing   *
 *  official policies, either expressed or implied, of Christian Krause.     *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


package scalax
package pecs

object Data {
  val folder   = "Folder"
  val document = "Document"
}

sealed trait Data {
  def title: String
  def id: String
  def description: String
  def subject: List[String]
  def language: String
  def creationDate: DateTime
  def modificationDate: DateTime
  def creators: List[String]
  def contributors: List[String]
  def allowDiscussion: Boolean
}

case class Folder (
    title: String,
    id: String,
    description: String,
    subject: List[String],
    language: String,
    creationDate: DateTime,
    modificationDate: DateTime,
    creators: List[String],
    contributors: List[String],
    allowDiscussion: Boolean
  ) extends Data

case class Document (
    title: String,
    id: String,
    description: String,
    subject: List[String],
    language: String,
    creationDate: DateTime,
    modificationDate: DateTime,
    creators: List[String],
    contributors: List[String],
    allowDiscussion: Boolean,
    hasTOC: Boolean,
    hasPresentation: Boolean,
    text: String
  ) extends Data
