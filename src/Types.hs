module Types where

newtype Name = Name { unName :: String }
  deriving (Eq, Ord)
newtype Hash = Hash String
  deriving (Eq, Ord)
newtype SourceCode = SourceCode String
  deriving (Eq, Show)

