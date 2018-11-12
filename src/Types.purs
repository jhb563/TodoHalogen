module Types where

import Prelude

newtype Todo = Todo
  { todoName :: String }

derive instance eqTodo :: Eq Todo
derive instance ordTodo :: Ord Todo
