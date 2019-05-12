module App.Formatter(
    formatWithTime
  ) where

import App.Shared

import Data.Char
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Text.Read
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv as Csv
import qualified Data.Vector as V

formatWithTime :: Text -> Text
formatWithTime = either pack formatWithTime' . Csv.decodeWith opts Csv.NoHeader . BS.fromStrict . encodeUtf8
  where
    opts = Csv.defaultDecodeOptions {
        Csv.decDelimiter = fromIntegral (ord ';')
      }

formatWithTime' :: Vector (Vector Text) -> Text
formatWithTime' = encodeWithTime . parseWithTimeState TimeParse V.empty

type WithTimeModel = Vector WithTimeRow

data WithTimeRow = WithTimeRow {
  wtrTime :: !Text
, wtrMeasures :: !(Vector Measure)
}

data Measure = Measure {
  measureSensor :: !Int
, measureValue  :: !Int
}

encodeWithTime :: WithTimeModel -> Text
encodeWithTime m = T.unlines $ header:datum
  where
    header = T.intercalate ";" ("Дата":"W0":"W1":[])
    datum = concat . V.map f $ m

    f :: WithTimeRow -> [Text]
    f WithTimeRow{..} = flip fmap (V.toList wtrMeasures) $ \Measure{..} -> T.intercalate ";" $
        [wtrTime] <> (replicate measureSensor "") <> [showt measureValue]

data WithTimeState = TimeParse | MeasureLabelParse !Text !(Vector Measure) | MeasureValueParse !Text !Int !(Vector Measure)

parseWithTimeState :: WithTimeState -> WithTimeModel -> Vector (Vector Text) -> WithTimeModel
parseWithTimeState !st !m !as
  | V.null as = m
  | otherwise = case st of
    TimeParse | V.null a -> parseWithTimeState (MeasureLabelParse "" V.empty) m as'
    TimeParse -> parseWithTimeState (MeasureLabelParse (V.head a) V.empty) m as'
    MeasureLabelParse !t !ms | V.null a -> parseWithTimeState TimeParse (V.snoc m $ WithTimeRow t ms) as'
    MeasureLabelParse !t !ms -> let
      v = T.toLower $ T.strip $ V.head a
      as'' = V.cons (V.tail a) as'
      in case v of
          "w" -> parseWithTimeState (MeasureLabelParse t ms) m as''
          "0=" -> parseWithTimeState (MeasureValueParse t 0 ms) m as''
          "1=" -> parseWithTimeState (MeasureValueParse t 1 ms) m as''
          "2=" -> parseWithTimeState (MeasureValueParse t 2 ms) m as''
          "3=" -> parseWithTimeState (MeasureValueParse t 3 ms) m as''
          _ -> parseWithTimeState (MeasureLabelParse t ms) m as''
    MeasureValueParse !t _  !ms | V.null a -> parseWithTimeState TimeParse (V.snoc m $ WithTimeRow t ms) as'
    MeasureValueParse !t !i !ms -> let
      v = T.toLower $ T.strip $ V.head a
      as'' = V.cons (V.tail a) as'
      in case signed decimal v of
          Left er -> parseWithTimeState (MeasureValueParse t i ms) m as''
          Right (vi, _) -> parseWithTimeState (MeasureLabelParse t (V.snoc ms $ Measure i vi)) m as''
    where
      as' = V.tail as
      a = V.head as
