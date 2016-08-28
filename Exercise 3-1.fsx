(*
    Exercise 3-1, 3.6 from Functional Programming Using F#
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

type Ampm =
    | AM
    | PM
type TimeTuple = byte * byte * Ampm
type TimeRecord =
    { hours: byte;
    minutes: byte;
    ampm: Ampm }

let areHoursInvalid h = h < 1uy || h > 12uy
let areMinutesInvalid m = m < 0uy || m > 59uy

let invalidHours = "Expected `hours` to be in range [1, 12]"
let invalidMinutes = "Expected `minute` to be in range [0, 59]"

let createTimeTuple h m ampm : TimeTuple =
    if areHoursInvalid h then failwith invalidHours
    else if areMinutesInvalid m then failwith invalidMinutes
    (h, m, ampm)

let (<<<) (t1: TimeTuple) (t2: TimeTuple) =
    let h1, m1, ampm1 = t1
    let h2, m2, ampm2 = t2
    ampm1 < ampm2
 || ampm1 = ampm2
    && (h1 = h2 && m1 < m2
        || h1 <> h2 && (h1 = 12uy || h2 <> 12uy && h1 < h2))

let createTimeRecord h m ampm : TimeRecord =
    if areHoursInvalid h then failwith invalidHours
    else if areMinutesInvalid m then failwith invalidMinutes
    { hours = h; minutes = m; ampm = ampm }

let (.<<<.) (t1: TimeRecord) (t2: TimeRecord) =
    let { hours = h1; minutes = m1; ampm = ampm1 } = t1
    let { hours = h2; minutes = m2; ampm = ampm2 } = t2
    ampm1 < ampm2
 || ampm1 = ampm2
     && (h1 = h2 && m1 < m2
        || h1 <> h2 && (h1 = 12uy || h2 <> 12uy && h1 < h2))
