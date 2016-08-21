(*
    Exercise 1-5 from Functional Programming Using F#
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

let rec calculateFibonacci = function
    |   0   ->  0
    |   1   ->  1
    |   n   ->  calculateFibonacci (n - 1) + calculateFibonacci (n - 2)

(*
    Evaluation for F 4:

        F 4 -> F (4 - 1) + F (4 - 2)
            -> F 3 + F 2
            -> F (3 - 1) + F (3 - 2) + F (2 - 1) + F (2 - 2)
            -> F 2 + F 1 + F 1 + F 0
            -> F (2 - 1) + F (2 - 2) + 1 + 1 + 0
            -> F 1 + F 0 + 2
            -> 1 + 0 + 2
            -> 3
*)