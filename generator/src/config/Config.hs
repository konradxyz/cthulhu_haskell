module Config where


data Config = Config { par :: Bool, tco :: Bool, move_opt :: Bool }
  deriving (Show, Eq, Ord)
