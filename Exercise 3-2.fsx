(*
    Exercise 3-2 from Functional Programming Using F#
    Copyright (c) 2016 Răzvan Mocanu

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

let shillingsToPound = 20
let penceToShilling = 12

type AmountTuple = int * int * int

let rec normalizeAmountTuple = function
    | ( pounds, shillings, pence )
        when
            pence >= penceToShilling
        ->
            normalizeAmountTuple (
                pounds,
                shillings + pence / penceToShilling,
                pence % penceToShilling
                )
    | ( pounds, shillings, pence )
        when
            shillings >= shillingsToPound
        ->
            normalizeAmountTuple (
                pounds + shillings / shillingsToPound,
                shillings % shillingsToPound,
                pence
                )
    | ( pounds, shillings, pence )
        when
            pence <= -penceToShilling
         || pence < 0 && (shillings > 0 || pounds > 0)
        ->
            normalizeAmountTuple (
                pounds,
                shillings - 1,
                penceToShilling + pence
                )
    | ( pounds, shillings, pence )
        when
            shillings <= -shillingsToPound
         || shillings < 0 && pounds > 0
        ->
            normalizeAmountTuple (
                pounds - 1,
                shillingsToPound + shillings,
                pence
                )
    | amount -> amount

let (+++) (x: AmountTuple) (y: AmountTuple) =
    let xPounds, xShillings, xPence = x
    let yPounds, yShillings, yPence = y
    normalizeAmountTuple (xPounds + yPounds, xShillings + yShillings, xPence + yPence)

let (---) (x: AmountTuple) (y: AmountTuple) =
    let xPounds, xShillings, xPence = x
    let yPounds, yShillings, yPence = y
    normalizeAmountTuple (xPounds - yPounds, xShillings - yShillings, xPence - yPence)

type AmountRecord =
    { pounds: int;
    shillings: int;
    pence: int }

let rec normalizeAmountRecord = function
    | { pounds = pounds; shillings = shillings; pence = pence }
        when
            pence >= penceToShilling
        ->
            normalizeAmountRecord {
                pounds = pounds
                shillings = shillings + pence / penceToShilling
                pence = pence % penceToShilling
                }
    | { pounds = pounds; shillings = shillings; pence = pence }
        when
            shillings >= shillingsToPound
        ->
            normalizeAmountRecord {
                pounds = pounds + shillings / shillingsToPound
                shillings = shillings % shillingsToPound
                pence = pence
                }
    | { pounds = pounds; shillings = shillings; pence = pence }
        when
            pence <= -penceToShilling
         || pence < 0 && (shillings > 0 || pounds > 0)
        ->
            normalizeAmountRecord {
                pounds = pounds
                shillings = shillings - 1
                pence = penceToShilling + pence
                }
    | { pounds = pounds; shillings = shillings; pence = pence }
        when
            shillings <= -shillingsToPound
         || shillings < 0 && pounds > 0
        ->
            normalizeAmountRecord {
                pounds = pounds - 1
                shillings = shillingsToPound + shillings
                pence = pence
                }
    | amount -> amount

let (.+++.) (x: AmountRecord) (y: AmountRecord) =
    let { pounds = xPounds; shillings = xShillings; pence = xPence } = x
    let { pounds = yPounds; shillings = yShillings; pence = yPence } = y
    normalizeAmountRecord {
        pounds = xPounds + yPounds
        shillings = xShillings + yShillings
        pence = xPence + yPence
        }

let (.---.) (x: AmountRecord) (y: AmountRecord) =
    let { pounds = xPounds; shillings = xShillings; pence = xPence } = x
    let { pounds = yPounds; shillings = yShillings; pence = yPence } = y
    normalizeAmountRecord {
        pounds = xPounds - yPounds
        shillings = xShillings - yShillings
        pence = xPence - yPence
        }
