module Counter where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Int

data Query a =
  Increment a |
  Decrement a |
  GetVal (Int -> a)

data Message = Updated Int

myCounter :: forall m. H.Component HH.HTML Query Unit Message m
myCounter = H.component
  { initialState: const 0
  , render
  , eval
  , receiver: const Nothing
  }

  where
    render :: State -> H.ComponentHTML Query
    render state =
      let label = show state
          incButton = HH.button [HP.title "Inc", HE.onClick (HE.input_ Increment)] [HH.text "Inc"]
          decButton = HH.button [HP.title "Dec", HE.onClick (HE.input_ Decrement)] [HH.text "Dec"]
          pElement  = HH.p [] [HH.text label]
      in  HH.div [] [incButton, decButton, pElement]  

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Increment next -> do
        state <- H.get
        let nextState = state + 1
        H.put nextState
        H.raise $ Updated nextState
        pure next
      Decrement next -> do
        state <- H.get
        let nextState = state - 1
        H.put nextState
        H.raise $ Updated nextState
        pure next
      GetVal reply -> do
        state <- H.get
        pure (reply state)
