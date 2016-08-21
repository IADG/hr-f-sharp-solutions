(*
    Exercise 2-7 from Functional Programming Using F#
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

let doesNotDivide (d, n) = n % d <> 0

let rec doesNotDivideRange (first, last, n) =
    first > last
    || doesNotDivide (first, n) && doesNotDivideRange (first + 1, last, n)

let isPrime n = n > 1 && doesNotDivideRange (2, n - 1, n)

let rec findNextPrime n =
    if isPrime <| n + 1
    then n + 1
    else findNextPrime <| n + 1
