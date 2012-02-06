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
import dispatch.json._
import dispatch.json.JsHttp._

/** Pecs resource using JSON as the data structure to retrieve information. */
case class JsonPecsResource(website: String, name: String = "")
  extends PecsResource[JsValue,JsonPecsResource] {

  // -----------------------------------------------------------------------
  // info request
  // -----------------------------------------------------------------------

  override protected def infoRequest = url(website + "/api_information_json")

  override protected def info = {
    val http = new Http with NoLogging
    val info = http(infoRequest ># identity)
    http.shutdown()
    info
  }

  override def views = info match {
    case JsObject(info) =>
      info get JsString("api_views") collect {
        case JsArray(a) => a collect { case JsString(s) => s }
      } getOrElse Nil

    case _ => Nil
  }

  // -----------------------------------------------------------------------
  // info
  // -----------------------------------------------------------------------

  override protected def resourceRequest = url(website + "/to_json")

  override protected def resource = {
    val http = new Http with NoLogging
    val res  = http(resourceRequest ># identity)
    http.shutdown()
    res
  }

  override def children = resource match {
    case JsObject(m) => m
      m.get(JsString("children")) collect {
        case JsObject(m) => m collect {
          case (JsString(name),JsString(path)) => JsonPecsResource(host + path, name)
        } toList
      } getOrElse Nil

    case _ => Nil
  }

}
