(*
    Exercise 2-1 from Functional Programming Using F#
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
    Declare a function f: int -> bool, such that f(n) = true exactly when n is
    divisible by 2 or divisible by 3 but not divisible by 5. Write down the
    expected values of f(24), f(27), f(29) and f(30) and compare with the result.
    Hint: n is divisible by q when n % q = 0.
*)

let f n = (n % 2 = 0 || n % 3 = 0) && n % 5 <> 0;;

f 24;;
f 27;;
f 29;;
f 30;;