module HSTypes where

type Height = Int
type Width = Int
type Dimensions = (Width, Height)

type X = Float
type Y = Float
type Coords = (X, Y)

type Image = [String]

type Object = (Coords, Dimensions, Image)
