(*
    Exercise 4-19 from Functional Programming Using F#
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

type Country = string
type Map = (Country * Country) list

let rec areNeighbours map c1 c2 =
    match map with
    | (n1, n2) :: ns ->
        (n1, n2) = (c1, c2) || (n1, n2) = (c2, c1) || areNeighbours ns c1 c2
    | [] -> false