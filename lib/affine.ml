open Base

type t = {
  translation : Point3.t;
  linear_transformation : Matrix.t
}

let apply {translation; linear_transformation} p =
  p
  |> Matrix.apply linear_transformation
  |> Point3.add translation

let compose {translation = c1; linear_transformation = f1} {translation = c2; linear_transformation = f2} =
  {
    linear_transformation = Matrix.compose f1 f2;
    translation = Point3.add (Matrix.apply f1 c2) c1
  }

let id = {translation = Point3.origin; linear_transformation = Matrix.id}
