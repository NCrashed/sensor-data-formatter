module App.Frontend(
    frontend
  ) where

import App.Shared
import Control.Monad.Fix
import Data.Functor
import Data.Text (Text)
import Reflex.Dom

type MonadFront t m = (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m, DomBuilderSpace m ~ GhcjsDomSpace)

frontend :: MonadFront t m => m ()
frontend = do
  el "div" $ do
    el "h4" $ text "Лог с датами"
    transformerWithTime

transformerWithTime :: MonadFront t m => m ()
transformerWithTime = mdo
  elClass "div" "row" $ do
    txtInD <- elClass "div" "column" textAreaIn
    elClass "div" "column" $ textAreaOut $ tag (current txtInD) btnE
  btnE <- button "Обработать"
  pure ()

textAreaIn :: MonadFront t m => m (Dynamic t Text)
textAreaIn = do
  ta <- textArea def
  pure $ _textArea_value ta

textAreaOut :: MonadFront t m => Event t Text -> m ()
textAreaOut txtE = void $ textArea def {
    _textAreaConfig_setValue = txtE
  }
