
module Format (
  Format(..)
  ) where

class Format a where
  format::a->String
  
