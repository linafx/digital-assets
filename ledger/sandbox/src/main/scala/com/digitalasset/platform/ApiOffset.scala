// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform

import com.daml.ledger.participant.state.v1.Offset
import com.digitalasset.daml.lf.data.Ref

import scala.util.Try

object ApiOffset {

  private val Bytes = 16
  private val MaxValue = BigInt(Array.fill(Bytes)(0xff.toByte).updated(0, 0x7f.toByte))
  private val Base10Digits = MaxValue.toString.length
  private val InvalidOffsetErrorMessage = s"Offset is negative or does not fit in $Bytes bytes"

  def fromString(s: String): Try[Offset] =
    Try {
      val asBigInt = BigInt(s)
      val unpadded = asBigInt.toByteArray
      require(asBigInt >= 0 && unpadded.length <= Bytes, InvalidOffsetErrorMessage)
      val padded = Array.ofDim[Byte](Bytes)
      Array.copy(
        src = unpadded,
        srcPos = 0,
        dest = padded,
        destPos = Bytes - unpadded.length,
        length = unpadded.length,
      )
      Offset.fromByteArray(padded)
    }

  def assertFromString(s: String): Offset = fromString(s).get

  def toApiString(offset: Offset): Ref.LedgerString = {
    val asBigInt = BigInt(offset.toByteArray)
    require(asBigInt >= 0 && asBigInt <= MaxValue, InvalidOffsetErrorMessage)
    val unpadded = asBigInt.toString
    Ref.LedgerString.assertFromString(("0" * (Base10Digits - unpadded.length)) + unpadded)
  }

  implicit class ApiOffsetConverter(val offset: Offset) {
    def toApiString: Ref.LedgerString = ApiOffset.toApiString(offset)
  }

}
