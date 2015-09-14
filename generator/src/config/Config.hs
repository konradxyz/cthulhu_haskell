module Config where


data Config = Config { par :: Bool, tco :: Bool }
  deriving (Show, Eq, Ord)
