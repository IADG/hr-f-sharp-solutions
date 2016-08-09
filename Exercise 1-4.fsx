(*
    Exercise 1-4 from Functional Programming Using F#
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

let rec f = function
    |   0   ->  0
    |   n   ->  f (n - 1) + n
;;

(*
    Recursion formula:

        f(0) = 0
        f(n) = f(n - 1) + n, n > 0

    Evaluation for f 4:

        f 4 -> f (4 - 1) + 4
            -> f 3 + 4
            -> f (3 - 1) + 3 + 4
            -> f 2 + 3 + 4
            -> f (2 - 1) + 2 + 3 + 4
            -> f 1 + 2 + 3 + 4
            -> f (1 - 1) + 1 + 2 + 3 + 4
            -> f 0 + 1 + 2 + 3 + 4
            -> 0 + 1 + 2 + 3 + 4
            -> 10
*)