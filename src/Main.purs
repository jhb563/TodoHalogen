module Main where

import Prelude

import Data.Either (Either(..))
-- import WebSender (webSender)
-- import Router (AppRoute(..), router)
-- import NavComponents (homeComponent, loginComponent, profileComponent, articleComponent)
import Parent (parentComponent, ParentQuery(..))
import NavComponents (Page(..))


import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Aff.Class (liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window

import Router (router)
import Routing (match)

hashChangeProducer :: CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer = CRA.produce \emitter -> do
  listener <- DOM.eventListener (traverse_ (CRA.emit emitter) <<< HCE.fromEvent)
  liftEffect $
    DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

hashChangeConsumer
  :: (forall a. ParentQuery a -> Aff a)
  -> CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
      result = match router hash
      newPage = case result of
                  Left _ -> HomePage
                  Right page -> page
  void $ liftAff $ query $ (H.action (ChangePage newPage) :: ParentQuery Unit)
  pure Nothing

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI parentComponent unit body
  CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)

{-
main :: Effect Unit
main = do
  nav <- makeInterface
  nav # matches router \_ newRoute -> HA.runHalogenAff do
    body <- HA.awaitBody
    let component = case newRoute of
          HomePage -> homeComponent
          LoginPage -> loginComponent
          ProfilePage userId -> profileComponent userId
          urticlePage userId articleId -> articleComponent userId articleId
    runUI component unit body
  effect
-}
