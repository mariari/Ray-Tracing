open Core

open Vectors

module Foo = Utilites.T(Vec3);;


let foo = Foo.unit (Vec3.create ~x:3. ~y:4. ~z:5.);;
