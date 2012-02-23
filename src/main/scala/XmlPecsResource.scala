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


package scalax.pecs

import dispatch._
import org.scala_tools.time.Implicits._
import scala.xml._

/** Pecs resource using XML as the data structure to retrieve information. */
case class XmlPecsResource(website: String, name: String = "")
  extends PecsResource[NodeSeq,XmlPecsResource] {

  // -----------------------------------------------------------------------
  // info request
  // -----------------------------------------------------------------------

  override protected def infoRequest = url(website + "/api_information_xml")

  override def info = {
    val http = new Http with NoLogging
    val info = http(infoRequest <> identity)
    http.shutdown()
    info
  }

  override def views = info \ "api_views" \ "api_view" map { _ text } toList

  // -----------------------------------------------------------------------
  // resource request
  // -----------------------------------------------------------------------

  override protected def resourceRequest = url(website + "/to_xml")

  override def resource = {
    val http = new Http with NoLogging
    val res  = http(resourceRequest <> identity)
    http.shutdown()
    res
  }

  override def resourceType = resource \ "type" text match {
    case "" => None
    case t  => Some(t)
  }

  override protected def dataValues = resource \ "values"

  private def convertDateString(s: String) =
    s.replaceAll("%3A",":").replaceAll("%2B","+")

  private def booleanOrFalse(s: String) = s match {
    case "" => false
    case s  => s.toBoolean
  }

  override def data = resourceType collect {
    case Data.folder => val values = dataValues ; Folder (
      values \ "title" text,
      values \ "id" text,
      values \ "description" text,
      values \ "subject" \ "value" map { _.text } toList,
      values \ "language" text,
      pecsDate(convertDateString(values \ "creation_date" text)),
      pecsDate(convertDateString(values \ "modification_date" text)),
      values \ "creators" \ "value" map { _.text } toList,
      values \ "contributors" \ "value" map { _.text } toList,
      booleanOrFalse(values \ "allowDiscussion" text)
    )

    case Data.document => val values = dataValues ; Document (
      values \ "title" text,
      values \ "id" text,
      values \ "description" text,
      values \ "subject" \ "value" map { _.text } toList,
      values \ "language" text,
      pecsDate(convertDateString(values \ "creation_date" text)),
      pecsDate(convertDateString(values \ "modification_date" text)),
      values \ "creators" \ "value" map { _.text } toList,
      values \ "contributors" \ "value" map { _.text } toList,
      booleanOrFalse(values \ "allowDiscussion" text),
      booleanOrFalse(values \ "tableContents" text),
      booleanOrFalse(values \ "presentation" text),
      values \ "text" text
    )
  }

  override def children = resource \\ "child" map { child =>
    XmlPecsResource(host + (child \ "url" text), child \ "title" text)
  } toList

}
