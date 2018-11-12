module TodoList where

import Prelude

import Data.Array (filter, snoc)
import Data.Maybe

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import TodoForm (addTodoForm, AddTodoFormQuery(..), AddTodoFormMessage(..))
import Types (Todo(..))

data TodoListQuery a =
  FinishedTodo Todo a |
  HandleNewTask AddTodoFormMessage a

type TodoListState = Array Todo

type AddTodoFormSlot = String

todoList :: forall m. Applicative m => H.Component HH.HTML TodoListQuery Unit Void m
todoList = H.parentComponent
  { initialState: const []
  , render
  , eval
  , receiver: const Nothing
  }

  where
render :: TodoListState -> H.ParentHTML TodoListQuery AddTodoFormQuery AddTodoFormSlot m
render todos =
  let taskList = HH.ul_ (map renderTask todos)
  in  HH.div_ [taskList, formSlot]

    renderTask :: Todo -> H.ParentHTML TodoListQuery AddTodoFormQuery AddTodoFormSlot m
    renderTask (Todo t) = HH.div_ 
      [ HH.p [] [HH.text t.todoName]
      , HH.button [HE.onClick (HE.input_ (FinishedTodo (Todo t)))] [HH.text "Finish"]
      ]

    formSlot :: H.ParentHTML TodoListQuery AddTodoFormQuery AddTodoFormSlot m
    formSlot = HH.slot "Add Todo Form" addTodoForm unit (HE.input HandleNewTask)

    eval :: TodoListQuery ~> H.ParentDSL TodoListState TodoListQuery AddTodoFormQuery AddTodoFormSlot Void m
    eval = case _ of
      FinishedTodo todo next -> do
        currentTasks <- H.get
        H.put (filter  (_ /= todo) currentTasks)
        pure next
      HandleNewTask (NewTodo todo) next -> do
        currentTasks <- H.get
        H.put (currentTasks `snoc` todo)
        pure next
