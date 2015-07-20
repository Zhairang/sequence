import Command

import Sequence

welcome = "Hello!\n"

main = do
  putStrLn welcome
  mainloop

mainloop = do
  input <- getLine
  modified <- eval (read input)
  putStrLn (show modified)
  mainloop
  
  
