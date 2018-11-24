module NavComponents where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ChildState = Int
data ChildQuery a = ChildQuery a
data ChildMessage = ChildMessage

data PageInfo = PageInfo String String String

data Page =
  HomePage |
  LoginPage |
  ProfilePage Int |
  ArticlePage Int String

homeComponent :: forall m. H.Component HH.HTML ChildQuery Unit ChildMessage m
homeComponent = H.component
  { initialState: const 0
  , render: renderF HomePage
  , eval
  , receiver: const Nothing
  }

loginComponent :: forall m. H.Component HH.HTML ChildQuery Unit ChildMessage m
loginComponent = H.component
  { initialState: const 0
  , render: renderF LoginPage
  , eval
  , receiver: const Nothing
  }

profileComponent :: Int -> forall m. H.Component HH.HTML ChildQuery Unit ChildMessage m
profileComponent uid = H.component
  { initialState: const 0
  , render: renderF (ProfilePage uid)
  , eval
  , receiver: const Nothing
  }

articleComponent :: Int -> String -> forall m. H.Component HH.HTML ChildQuery Unit ChildMessage m
articleComponent uid aid = H.component
  { initialState: const 0
  , render: renderF (ArticlePage uid aid)
  , eval
  , receiver: const Nothing
  }

renderF :: Page -> ChildState -> H.ComponentHTML ChildQuery
renderF page _ = HH.div [] 
  [ HH.h1 [] [HH.text title]
  , HH.a [HP.href link] [HH.text nextPage]
  ]
  where
    PageInfo title link nextPage = case page of
      HomePage -> PageInfo "Home Page!" "#login" "Go Login!"
      LoginPage -> PageInfo "Login Page!" "#profile/1" "View a Profile!"
      ProfilePage uid -> PageInfo ("Profile for: " <> show uid) "#blog/articles/1/hellothere" "See an Article!"
      ArticlePage uid aid -> PageInfo ("Article " <> aid <> " by: " <> show uid) "#home" "Go Home!"

eval :: forall m. ChildQuery ~> H.ComponentDSL ChildState ChildQuery ChildMessage m
eval = case _ of
  ChildQuery next -> pure next
