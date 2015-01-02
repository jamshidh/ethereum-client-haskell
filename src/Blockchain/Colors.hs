{-# OPTIONS_GHC  -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Blockchain.Colors (
    red, green, yellow, blue,
    magenta, cyan, white2, white, black,
    bright, dim, underline, blink, Blockchain.Colors.reverse, hidden
) where


bright string = "\ESC[1m" ++ string ++ "\ESC[0m"
dim string = "\ESC[2m" ++ string ++ "\ESC[0m"
underline string = "\ESC[4m" ++ string ++ "\ESC[0m"
blink string = "\ESC[5m" ++ string ++ "\ESC[0m"
reverse string = "\ESC[7m" ++ string ++ "\ESC[0m"
hidden string = "\ESC[8m" ++ string ++ "\ESC[0m"


black string = "\ESC[30m" ++ string ++ "\ESC[0m"
red string = "\ESC[31m" ++ string ++ "\ESC[0m"
green string = "\ESC[32m" ++ string ++ "\ESC[0m"
yellow string = "\ESC[33m" ++ string ++ "\ESC[0m"
blue string = "\ESC[34m" ++ string ++ "\ESC[0m"
magenta string = "\ESC[35m" ++ string ++ "\ESC[0m" --AKA purple
cyan string = "\ESC[36m" ++ string ++ "\ESC[0m" --AKA aqua
white string = "\ESC[37m" ++ string ++ "\ESC[0m"
white2 string = "\ESC[38m" ++ string ++ "\ESC[0m"
