module Handler.Items where

import Import

itemForm :: Form Item
itemForm = renderDivs $ Item
           <$> areq textField "Name" Nothing
           <*> areq textField "Stock Number" Nothing

getItemsR :: Handler Html
getItemsR = do
  items <- runDB $ selectList [] []
  (itemWidget, enctype) <- generateFormPost itemForm
  defaultLayout $(widgetFile "index")

postItemsR :: Handler Html
postItemsR = do
  ((res, _), _) <- runFormPost itemForm
  case res of
    FormSuccess item -> do
      _ <- runDB $ insert item
      setMessage $ toHtml $ (itemName item) <> " created"
      getItemsR 
    _ -> do
      setMessage $ toHtml $ ("correct item field" :: String)
      getItemsR
