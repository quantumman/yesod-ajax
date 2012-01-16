{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances #-}

import Yesod
import Yesod.Form.Jquery
-- import Yesod.Handler (Method (..))
import Text.Julius
import Data.Text hiding (toLower, map)
import Data.Char (toLower)
import Control.Applicative

data Ajax = Ajax


mkYesod "Ajax" [parseRoutes|
/ RootR GET POST
|]


instance Yesod Ajax where
    approot _ = ""


instance RenderMessage Ajax FormMessage where
    renderMessage = renderMessage


instance YesodJquery Ajax where


data Person = Person
    { name :: Text }


personForm :: AForm Ajax Ajax Person
personForm = Person <$> areq textField "Name" Nothing

renderedPersonForm :: Html -> MForm Ajax Ajax (FormResult Person, Widget)
renderedPersonForm = renderDivs $ personForm


data Method = GET | POST deriving(Show)
data SubmitForm xml = SubmitForm
                  {
                    method :: Method
                  , enctype :: Enctype
                  , form :: xml
                  }


remoteFormTag :: SubmitForm (GWidget sub master a) -> Route master -> GWidget sub master ()
remoteFormTag (SubmitForm method enctype form) route = do
  let show' = map toLower . show
  divId    <- lift newIdent
  formId   <- lift newIdent
  submitId <- lift newIdent
  addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
  addJulius [julius|
$(function() {
    $("form##{formId} > input##{submitId}").click(function(){
    $.#{show' method}($(this).attr("action"), $("form##{formId}").serializeArray(),
       function(data) {
          $("div##{divId}").html(data);
       });
       return false;
    });
});
|]
  addWidget [whamlet|
<div id="#{divId}">
  <form method="#{show' method}" enctype="#{enctype}" action="@{route}" id="#{formId}">
          ^{form}
          <input type=submit id="#{submitId}">
|]


formTag :: SubmitForm (GWidget sub master a) -> Route master -> GWidget sub master ()
formTag (SubmitForm method enctype form) route = do
  let show' = map toLower . show
  addWidget [whamlet|
<form method="#{show' method}" enctype="#{enctype}" action="@{route}">
          ^{form}
          <input type=submit>
|]


generateFormPost' :: (RenderMessage master FormMessage)
                 => (Html -> MForm sub master (FormResult a, xml))
                 -> GHandler sub master (SubmitForm xml)
generateFormPost' form = do
  ((_, form), enctype) <- generateFormPost form
  return $ SubmitForm POST enctype form

generateFormGet' :: (RenderMessage master FormMessage)
                 => (Html -> MForm sub master (a, xml))
                 -> GHandler sub master (SubmitForm xml)
generateFormGet' form = do
  ((_, form), enctype) <- generateFormGet form
  return $ SubmitForm GET enctype form


getRootR :: Handler RepHtml
getRootR = do
  postForm <- generateFormPost' renderedPersonForm
  defaultLayout $ do
             addWidget [whamlet|
$with remote_form_tag <- remoteFormTag postForm RootR
 ^{remote_form_tag}
 <p>If Ajax works well, then you will see this sentence!
|]



postRootR :: Handler RepHtml
postRootR = do
  ((result, _), _) <- runFormPost renderedPersonForm
  case result of
    FormSuccess person -> defaultLayout [whamlet| <p>Post succeeded: #{name person} |]
    _ -> defaultLayout [whamlet| <p>Post was failure |]


main = warpDebug 3000 Ajax


