(*
    Exercise 4-11 from Functional Programming Using F#
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

let rec count (xs, x) =
    match xs with
    | y :: ys when y < x -> count (ys, x)
    | y :: ys when y = x -> 1 + count (ys, x)
    | _ -> 0

let rec insert (xs, x) =
    match xs with
    | y :: _ as ys when x <= y -> x :: ys
    | y :: ys -> y :: insert (ys, x)
    | _ -> [x]

let rec intersect = function
    | (x :: xs, y :: ys) when x = y -> x :: intersect (xs, ys)
    | (x :: xs, (y :: _ as ys)) when x < y -> intersect (xs, ys)
    | ((x :: _ as xs), y :: ys) when x > y -> intersect (xs, ys)
    | _ -> []

let rec plus = function
    | (x :: xs, (y :: _ as ys)) when x < y -> x :: plus (xs, ys)
    | ((x :: _ as xs), y :: ys) when x > y -> y :: plus (xs, ys)
    | (x :: xs, y :: ys) when x = y -> x :: y :: plus (xs, ys)
    | ([], ys) -> ys
    | (xs, []) -> xs
    | _ -> []