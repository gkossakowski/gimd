// Copyright (C) 2009 The Android Open Source Project
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package com.google.gimd

import java.util.Date
import java.util.TimeZone
import com.google.gimd.text.Formatter

object Timestamp {
  def now: Timestamp = apply(System.currentTimeMillis, localOffset)
  private val localOffset = TimeZone.getDefault.getRawOffset

  def apply(time: Date): Timestamp              = apply(time, 0)
  def apply(time: Date, offset: Int): Timestamp = apply(time.getTime, offset)
  def apply(time: Long): Timestamp              = apply(time, 0)
  def apply(time: Long, offset: Int): Timestamp = new Timestamp(time, offset)
}

/**
 * Date and time value with a matching timezone offset.
 * 
 * @param millisSinceEpoch number of milliseconds since the Java epoch.
 * @param offset number of minutes offset from UTC where the timestamp
 *      was sampled.  Negative values are west of UTC, positive values
 *      are east of UTC.
 */
final class Timestamp(
  millisSinceEpoch: Long,
  val offset: Int) extends Date(millisSinceEpoch) {

  /** Get the TimeZone this time stamp is located in. */
  def timeZone = {
    val offsetHours = offset.abs / 60
    val offsetMins = offset.abs % 60

    val tzId = new StringBuilder(8)
    tzId.append("GMT")
    tzId.append(if (offset < 0) '-' else '+');
    if (offsetHours < 10)
        tzId.append('0')
    tzId.append(offsetHours)
    if (offsetMins < 10)
        tzId.append('0');
    tzId.append(offsetMins)
    TimeZone.getTimeZone(tzId.toString())
  }

  override def equals(that: Any) = that match {
    case that: Timestamp =>
      this.getTime == that.getTime && this.offset == that.offset
    case _ => false
  }

  override def toString = Formatter.valueString(this)
}
