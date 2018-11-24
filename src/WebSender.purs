module WebSender where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Effect.Console (log)
import Effect.Class (liftEffect)

import Affjax as AX
import Affjax.ResponseFormat as AXR
import Affjax.RequestBody as AXRB
import Data.Argonaut.Core as JSON

type State =
  { getResponse :: String
  , postInfo :: String
  }

initialState :: State
initialState = 
  { getResponse: "Nothing Yet"
  , postInfo: ""
  }

data Query a =
  SendGet a |
  SendPost a |
  UpdatedPostInfo String a

data Message = ReceivedFromPost String

webSender :: H.Component HH.HTML Query Unit Message Aff
webSender = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

render :: State -> H.ComponentHTML Query
render st = HH.div [] [progressText, getButton, inputText, postButton]
  where
    progressText = HH.p [] [HH.text st.getResponse]
    getButton = HH.button
      [ HP.title "Send Get", HE.onClick (HE.input_ SendGet) ]
      [ HH.text "Send Get" ]
    inputText = HH.input
      [ HP.type_ HP.InputText
      , HP.placeholder "Form Data"
      , HP.value st.postInfo
      , HE.onValueChange (HE.input UpdatedPostInfo)
      ]
    postButton =  HH.button
      [ HP.title "Send Post", HE.onClick (HE.input_ SendPost) ]
      [ HH.text "Send Post" ]

eval :: Query ~> H.ComponentDSL State Query Message Aff
eval = case _ of
  SendGet next -> do
    response <- H.liftAff $ AX.get AXR.string "http://localhost:8081/api/hello"
    st <- H.get
    case response.body of
      Right success -> H.put (st { getResponse = success })
      Left _ -> H.put (st { getResponse = "Error!" })
    liftEffect $ log "Hello"
    pure next
  SendPost next -> do
    st <- H.get
    let body = AXRB.json (JSON.fromString st.postInfo)
    response <- H.liftAff $ AX.post AXR.string "http://localhost:8081/api/post" body
    case response.body of
      Left _ -> H.raise (ReceivedFromPost "There was an error!")
      Right success -> H.raise (ReceivedFromPost success)
    pure next
  UpdatedPostInfo newInfo next -> do
    st <- H.get
    H.put (st { postInfo = newInfo })
    pure next
