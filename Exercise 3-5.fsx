(*
    Exercise 3-5 from Functional Programming Using F#
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

type Equation = float * float * float
type Solution =
    | Single of float
    | Pair of float * float

let solve (equation: Equation) =
    let a, b, c = equation
    let d = b * b - 4.0 * a * c
    if d < 0.0 || a = 0.0
    then None
    else
        let a' = 2.0 * a
        if d = 0.0
        then Some (Single (-b / a'))
        else
            let d' = sqrt d
            Some (Pair ( (-b + d') / a', (-b - d') / a' ))