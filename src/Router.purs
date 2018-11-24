module Router where

import Prelude
import Control.Alternative ((<|>))
import Routing.Match (Match, lit, int, str)
import NavComponents (Page(..))

pageFromHash :: String -> Page
pageFromHash "login" = LoginPage
pageFromHash "profile" = ProfilePage 1
pageFromHash "articles" = ArticlePage 1 "Hello"
pageFromHash _ = HomePage

router :: Match Page
router = matchHome <|> matchLogin <|> matchProfile <|> matchArticle

matchHome :: Match Page
matchHome = HomePage <$ lit "home"

matchLogin :: Match Page
matchLogin = LoginPage <$ lit "login"

matchProfile :: Match Page
matchProfile = ProfilePage <$> (lit "profile" *> int)

matchArticle :: Match Page
matchArticle = ArticlePage <$> (lit "blog" *> lit "articles" *> int) <*> str
