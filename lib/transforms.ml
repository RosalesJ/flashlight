open Base
open Float

type axis = X | Y | Z

let linear_transformation matrix =
  Affine.{translation = Point3.origin; linear_transformation = matrix}

let translation trans =
  Affine.{translation = trans; linear_transformation = Matrix.id}

let rotation_x angle =
  Matrix.make
    ((1.,      0.,           0.),
     (0.,  cos angle,  -.sin angle),
     (0.,  sin angle,    cos angle))
  |> linear_transformation

let rotation_y angle =
  Matrix.make
    ((  cos angle,  0.,  sin angle),
     (      0.,     1.,       0.),
     (-.sin angle,  0.,  cos angle))
  |> linear_transformation

let rotation_z angle =
  Matrix.make
    ((cos angle,  -.sin angle,   0.),
     (sin angle,    cos angle,   0.),
     (    0.,           0.,      1.))
  |> linear_transformation

let transform_about point transform =
  translation (Point3.neg point)
  |> Affine.compose transform
  |> Affine.compose (translation point)
