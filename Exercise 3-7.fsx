(*
    Exercise 3-7 from Functional Programming Using F#
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

type Shape =
    | Circle of float
    | Square of float
    | Triangle of float * float * float

let calculateArea = function
    | Circle r when r < 0.0 -> failwith "Expected radius to be > 0.0."
    | Circle r -> System.Math.PI * r * r
    | Square a when a < 0.0 -> failwith "Expected length to be > 0.0."
    | Square a -> a * a
    | Triangle ( a, b, c )
        when
            a < 0.0
         || b < 0.0
         || c < 0.0
         || a > b + c
         || b > c + a
         || c > a + b
        -> failwith ("Expected lengths to be > 0.0 and "
                + "triangle inequalities to be satisfied.")
    | Triangle ( a, b, c )->
        let s = (a + b + c) / 2.0
        sqrt (s * (s - a) * (s - b) * (s - c))
