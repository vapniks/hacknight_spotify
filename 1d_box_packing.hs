{-# LANGUAGE OverloadedStrings #-}

-- This JSON parsing code was written by Steve Purcell: https://gist.github.com/purcell/8974857
module SpotifyPack
where
import qualified Network.URI as URI
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Aeson (FromJSON, parseJSON, decode, (.:))
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import System.Environment

data Song = Song { label :: String, duration :: Float }
          deriving (Show, Eq, Ord)
 
searchURL :: String -> String
searchURL term = "http://ws.spotify.com/search/1/track.json?q=" ++
                 URI.escapeURIString (not . URI.isReserved) term
 
-- Fetch Songs from Spotify's search API
searchSpotify :: String -> IO (Maybe [Song])
searchSpotify term = do
  response <- simpleHTTP $ getRequest $ searchURL term
  body <- getResponseBody response
  return . parseResponse $ BSL.pack body
 
instance FromJSON Song where
  parseJSON (T.Object v) = Song <$> v .: "name" <*> v.: "length"
  parseJSON _ = mzero
 
-- Helped by http://www.the-singleton.com/2012/02/parsing-nested-json-in-haskell-with-aeson/
parseResponse :: BSL.ByteString -> Maybe [Song]
parseResponse resp =
    do result <- decode resp
       flip T.parseMaybe result $ \obj ->
           parseJSON =<< (obj .: "tracks")

-- Simple algorithm - just stuff the songs in until you can't fit any more
packSongs :: (Maybe [Song]) -> Float -> ((Maybe [Song]), Float)
packSongs _ 0 = (Nothing, 0)
packSongs (Just []) _ = (Nothing, 0)
packSongs Nothing _ = (Nothing, 0)
packSongs (Just songs) space =
  (Just (fst x), (snd x))
  where x = last $ takeWhile ((< space) . snd) $ scanl (\acc s -> (s:(fst acc),(snd acc)+(duration s))) ([], 0) songs

main = do
  (term:(length:_)) <- getArgs
  songs <- searchSpotify term
  print (snd (packSongs songs (read length::Float)))

-- A better solution is here: https://github.com/purcell/spotify-pack/blob/master/SpotifyPack.hs

