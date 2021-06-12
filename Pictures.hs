module Pictures where

type Picture = [String]

horse :: Picture
horse = [
    ".......##.....",
    ".....##..#....",
    "...##.....#...",
    "..#.......#...",
    "..#...#...#...",
    "..#...###.#...",
    ".#....#..##...",
    "..#...#.......",
    "...#...#......",
    "....#..#......",
    ".....#.#......",
    "......##......"
  ]

draw :: Picture -> IO()
draw pic = putStr (toString pic)

toString :: Picture -> String
toString = undefined

flipH :: Picture -> Picture
flipH = undefined

flipV :: Picture -> Picture
flipV = undefined

rotate :: Picture -> Picture
rotate pic = flipH (flipV pic)

invertColor :: Picture -> Picture
invertColor = undefined

whiteHorse :: Picture
whiteHorse = invertColor horse

above :: Picture -> Picture -> Picture
above = undefined

beside :: Picture -> Picture -> Picture
beside = undefined

superimpose :: Picture -> Picture -> Picture
superimpose = undefined

scale :: Picture -> Int -> Picture
scale = undefined
