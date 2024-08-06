module Ex01 where

import Codec.Picture (writePng)
import ShapeGraphics


-- PART 1 --
-- picture of a house

import Data.List (splitAt)

housePic :: Picture
housePic = [chimneyHouse, door, window]
  where
    houseCoords :: [Point]
    houseCoords = merge houseCOx houseCOy

    -- Split houseCoords after the first 4 points
    (firstPart, secondPart) = splitAt 4 houseCoords
    
    -- Convert chimneyCOs to points
    chimneyCoords :: [Point]
    chimneyCoords = toPoints chimneyCOs
    
    -- Concatenate the parts with the chimney in the middle
    combinedCoords :: [Point]
    combinedCoords = firstPart ++ chimneyCoords ++ secondPart
    
    chimneyHouse :: PictureObject
    chimneyHouse = Path combinedCoords green Solid
    
    door :: PictureObject
    door = Path (toPoints doorCOs) red Solid
    
    window :: PictureObject
    window = Polygon (toPoints windowCOs) cyan Solid SolidFill


-- Coord information

doorCOs :: [(Float, Float)]
doorCOs = [(550, 750), (550, 550), (650, 550), (650, 750)]

houseCOx :: [Float]
houseCOx = [300.0,300.0,270.0,500.0,730.0,700.0,700.0]
houseCOy :: [Float]
houseCOy = [750.0,450.0,450.0,200.0,450.0,450.0,750.0]

chimneyCOs :: [(Float, Float)]
chimneyCOs = [(605, 325), (605, 250), (650, 250), (650, 363)]

windowCOs :: [(Float, Float)]
windowCOs = [(350, 650), (350, 550), (450, 550), (450, 650)]

cyan :: Colour
cyan = Colour 96 192 255 255

-- Helper functions

toPoints :: [(Float, Float)] -> [Point]
toPoints = map (\(x, y) -> Point x y)

merge :: [Float] -> [Float] -> [Point]
merge xCoords yCoords = map (\(x, y) -> Point x y) (zip xCoords yCoords)





-- PART 2 --

movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)


movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points color lineStyle) = Path (map (flip movePoint vec) points) color lineStyle
movePictureObject vec (Circle centerPO radiusPO colourPO lineStylePO fillStylePO)  = Circle (movePoint centerPO vec) radiusPO colourPO lineStylePO fillStylePO


-- PART 3 --

-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic _ n | n < 0.01 = []
simpleCirclePic col n = Circle (Point 400 400) (400/n) col Solid NoFill : simpleCirclePic col (n/1.2)


-- EXAMPLE --

-- The following is an example picture, showing the usage of the
-- ShapeGraphics library. Use `writeToFile example` to view it.
example :: Picture
example = [redEllipse, blueEllipse] where
  redEllipse :: PictureObject
  redEllipse =
    Ellipse (Point 400 400) --center
            100             --width
            200             --height
            (pi/4)          --rotation (radians)
            white             --color
            Solid           --line (stroke) style
            SolidFill       --fill style
  blueEllipse :: PictureObject
  blueEllipse =
    Ellipse (Point 400 400) --center
            150             --width
            250             --height
            0.0             --rotation (radians)
            blue            --color
            Solid           --line (stroke) style
            NoFill          --fill style


writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)