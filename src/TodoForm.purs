module TodoForm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Types (Todo(..))

data AddTodoFormMessage = NewTodo Todo

type AddTodoFormState = String

data AddTodoFormQuery a = 
  AddedTodo a |
  UpdatedName String a

addTodoForm :: forall m. H.Component HH.HTML AddTodoFormQuery Unit AddTodoFormMessage m
addTodoForm = H.component
  { initialState: const ""
  , render
  , eval
  , receiver: const Nothing
  }

  where
    render :: AddTodoFormState -> H.ComponentHTML AddTodoFormQuery
    render currentName =
      let nameInput = HH.input
                        [ HP.type_ HP.InputText
                        , HP.placeholder "Task Name"
                        , HP.value currentName
                        , HE.onValueChange (HE.input UpdatedName)
                        ]
          addButton = HH.button
                        [ HP.title "Add Task"
                        , HP.disabled (length currentName == 0)
                        , HE.onClick (HE.input_ AddedTodo)
                        ]
                        [ HH.text "Add Task" ]
      in  HH.div [] [nameInput, addButton]

    eval :: AddTodoFormQuery ~> H.ComponentDSL AddTodoFormState AddTodoFormQuery AddTodoFormMessage m
    eval = case _ of
      AddedTodo next -> do
        currentName <- H.get
        H.put ""
        H.raise $ NewTodo (Todo {todoName: currentName})
        pure next
      UpdatedName newName next -> do
        H.put newName
        pure next
