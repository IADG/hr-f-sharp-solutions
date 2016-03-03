(*
    Exercise 1-6 from Functional Programming Using F#
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

(*
    Declare a recursive function sum: int * int -> int, where
        sum (m, n) = m + (m + 1) + (m + 2) + ... (m + (n - 1)) + (m + n)
    for m >= 0 and n >= 0. Hint: Use 2 clauses with (m, 0) and (m, n) as patterns.
*)

let rec sum = function
    |   (m, 0)  ->  m
    |   (m, n)  ->  sum (m, n - 1) + m + n
;;