import Command

import Sequence

welcome = "Hello!"

main = do
  putStrLn welcome
  putStrLn divider
  mainloop

mainloop = do
  input <- getLine
  modified <- eval (read input)
  putStrLn (show modified)
  putStrLn divider
  mainloop
  
divider = take 60 (repeat '*')
