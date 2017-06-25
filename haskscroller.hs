main = do
    calibrate
    printTitle
    return ()

--------- Game config
gwidth = 100
gheight = 50

---------- Intro

txtPrompt = "Welcome to Haskscroller! Please adjust the terminal size until this box is all that you can see, and press Enter to continue:"
btxtPrompt = border txtPrompt '|' '+' '-'

calibrate = do
    sequence (map putStrLn (head btxtPrompt))
    ready <- getLine
    return ()

txtTitle0 = "    __  __    _______   "
txtTitle1 = "   / | | |   /  ____|   "
txtTitle2 = "   |_|_|_|   |_/____    "
txtTitle3 = " --(_____()--(_____()-- "
txtTitle4 = "   | | | |   ____/  |   "
txtTitle5 = "   |_| |_/   |______/   "

printTitle = do
    putStrLn txtTitle0
    putStrLn txtTitle1
    putStrLn txtTitle2
    putStrLn txtTitle3
    putStrLn txtTitle4
    putStrLn txtTitle5
    return ()

---------- Text Helpers

split :: [a] -> Int -> ([a], [a])
split xs n
  = (take n xs, drop n xs)

section :: [a] -> Int -> [[a]]
section xs n
  = (xsfirst) : (section xsrem n)
  where
    (xsfirst, xsrem) = split xs n

pad :: String -> Char -> String
pad txt ch
  = take gwidth (txt ++ (repeat ch))

border :: String -> Char -> Char -> Char -> [[String]]
border txt side corner top
  = map (\body -> ([hb] ++ body ++ [hb])) bodies
  where
    hb = hborder corner top
    bodies = section (vborder txt side) gheight

hborder :: Char -> Char -> String
hborder corner top
  = [corner] ++ (pad "" top) ++ [corner]

vborder :: String -> Char -> [String]
vborder [] _ = []
vborder txt side
  = borderedtxt : (vborder rem side)
  where
    borderedtxt = side : (txt' ++ [side])
    txt' = pad txt ' '
    rem = drop gwidth txt
