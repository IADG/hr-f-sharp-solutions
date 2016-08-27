(*
    Exercise 3-3 from Functional Programming Using F#
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

type Complex = float * float

let (+^) (c1: Complex) (c2: Complex) =
    let r1, i1 = c1
    let r2, i2 = c2
    ( r1 + r2, i1 + i2 )

let ( *^) (c1: Complex) (c2: Complex) =
    let r1, i1 = c1
    let r2, i2 = c2
    ( r1 * r2 - i1 * i2, r1 * i2 + i1 * r2 )

let (-^) (c1: Complex) (c2: Complex) =
    let r2, i2 = c2
    c1 +^ ( -r2, -i2 )

let (/^) (c1: Complex) (c2: Complex) =
    let r2, i2 = c2
    let d = r2 ** 2. + i2 ** 2.
    c1 *^ (r2 / d, -i2 / d)