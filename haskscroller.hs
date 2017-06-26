import Data.String
import Data.List
import HSString
import HSDisplay
import HSObjects

main = do
    calibrate
    return ()

--------- Game config
gwidth = 80
gheight = 30

---------- Intro

txtPrompt = "Welcome to Haskscroller! Before we begin our quest, please adjust the terminal size until this box is all that you can see, and press Enter to continue."

txtLogo = "\n\n\n\n\n\
\ __  __    ______\n\
\/ | | |   / ____|\n\
\|_|_|_|  |_/____ \n\
\(_____() (_____()\n\
\| | | |   ____/ |\n\
\|_| |_/  |_____/ "

calibrate = do
    sequence (map putStrLn (border (gwidth, gheight) lineBorder ((txtformat gwidth txtPrompt) ++ (artformat gwidth txtLogo))))
    ready <- getLine
    return ()


---------- Text Helpers

section :: [a] -> Int -> [[a]]
section xs n
  = (xsfirst) : (section xsrem n)
  where
    (xsfirst, xsrem) = splitAt n xs
