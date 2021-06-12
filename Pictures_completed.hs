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
toString [] = ""
toString (l:ls) = l ++ "\n" ++ toString ls

flipH :: Picture -> Picture
flipH pic = reverse pic

flipV :: Picture -> Picture
flipV [] = []
flipV (l:ls) = reverse l : flipV ls

rotate :: Picture -> Picture
rotate pic = flipH (flipV pic)

invertColor :: Picture -> Picture
invertColor [] = []
invertColor (l:ls) = invertLine l : invertColor ls
    where
        invertLine :: String -> String
        invertLine [] = []
        -- takes a string, converts '.' characters to '#' and other way around
        invertLine (c:cs) = (if c == '.' then '#' else '.') : invertLine cs

whiteHorse :: Picture
whiteHorse = invertColor horse

above :: Picture -> Picture -> Picture
above p1 p2 = p1 ++ p2


beside :: Picture -> Picture -> Picture
beside (x:xs) (y:ys) = (x ++ " " ++ y) : beside xs ys
beside _ _ = []

superimpose :: Picture -> Picture -> Picture
superimpose (x:xs) (y:ys) = superimposeLine x y: superimpose xs ys
    where
        superimposeLine :: String -> String -> String
        superimposeLine cs ds = case (cs, ds) of
            ('.':cs', '.':ds') -> '.' : superimposeLine cs' ds'
            (_:cs', _:ds') -> '#' : superimposeLine cs' ds'
            ( _, _ ) -> []
superimpose _ _ = []


scale' :: Picture -> Int -> Picture
scale' [] _ = []
--scale pic 1 = pic
scale' (l:ls) n = replicate n (scaleLine l n) ++ scale' ls n
    where
        scaleLine :: String -> Int -> String
        scaleLine [] _ = []
        --scaleLine cs 1 = cs
        scaleLine (c:cs) m = replicate m c ++ scaleLine cs m


-- wrapper function
scale :: Picture -> Int -> Picture
scale pic n
    | n < 0 = error "negative scaling factor"
    | n == 0 = []
    | otherwise = scale' pic n
