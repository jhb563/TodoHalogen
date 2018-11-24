module Parent where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import NavComponents (Page(..), ChildQuery, Message)
import NavComponents as N

data ParentQuery a =
  ChangePage Page a |
  HandleAppAction Message a

type ParentState = { currentPage :: Page }

initialState :: ParentState
initialState = { currentPage: HomePage }

data SlotId = HomeSlot | LoginSlot | ProfileSlot | ArticleSlot

derive instance slotEq :: Eq SlotId
derive instance slotOrd :: Ord SlotId

parentComponent :: forall m. Applicative m => H.Component HH.HTML ParentQuery Unit Void m
parentComponent = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

render :: forall m. ParentState -> H.ParentHTML ParentQuery ChildQuery SlotId m
render st = HH.div_
  [ HH.button [HE.onClick (HE.input_ (ChangePage next))] [HH.text "Next"]
  , HH.slot sl comp unit (HE.input HandleAppAction)
  ]
  where
    next = nextPage st.currentPage
    sl = slotForPage st.currentPage
    comp = componentForPage st.currentPage

eval :: forall m. ParentQuery ~> H.ParentDSL ParentState ParentQuery ChildQuery SlotId Void m
eval = case _ of
  ChangePage pg next -> do
    H.put {currentPage: pg}
    pure next
  HandleAppAction _ next -> do
    pure next

nextPage :: Page -> Page
nextPage HomePage = LoginPage
nextPage LoginPage = ProfilePage 1
nextPage (ProfilePage _) = ArticlePage 1 "Hello"
nextPage (ArticlePage _ _) = HomePage

slotForPage :: Page -> SlotId
slotForPage HomePage = HomeSlot
slotForPage LoginPage = LoginSlot
slotForPage (ProfilePage _) = ProfileSlot
slotForPage (ArticlePage _ _) = ArticleSlot

componentForPage :: forall m. Page -> H.Component HH.HTML ChildQuery Unit Message m
componentForPage HomePage = N.homeComponent
componentForPage LoginPage = N.loginComponent
componentForPage (ProfilePage uid) = N.profileComponent uid
componentForPage (ArticlePage uid aid) = N.articleComponent uid aid
