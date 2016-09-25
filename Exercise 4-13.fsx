(*
    Exercise 4-13 from Functional Programming Using F#
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

let rec min (xs: int list): int =
    match xs with
    | [x] -> x
    | x :: xs ->
        let m = min xs
        if x < m then x else m
    | [] -> failwith "Expected list to be non-empty."

let rec delete (a: int, xs: int list) =
    match xs with
    | x :: xs when x = a -> xs
    | x :: xs -> x :: delete (a, xs)
    | _ -> []

let rec sort (xs: int list): int list =
    match xs with
    | x :: xs' ->
        let m = min xs
        m :: sort (delete (m, xs))
    | [] -> []