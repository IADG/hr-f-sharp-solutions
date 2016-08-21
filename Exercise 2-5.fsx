(*
    Exercise 2-5 from Functional Programming Using F#
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

let isCharAtIndex (s: string, i, c) = s.[i] = c

let countCharAtIndex a = if isCharAtIndex a then 1 else 0

let rec countOccurrencesFromIndex (s, i, c) =
    if i >= 0 && i < String.length s
    then countCharAtIndex (s, i, c) + countOccurrencesFromIndex (s, i + 1, c)
    else 0

let countOccurrencesInString (s, c) = countOccurrencesFromIndex (s, 0, c)
