module Archive
  ( buildArchivePaginateWith
  , archiveId
  , archiveGroup
  , yearForIdentifier
  ) where

import Data.List (groupBy)
import Data.Time.Calendar (toGregorian)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.LocalTime (localDay, utcToLocalTime, utc)
import Hakyll
import qualified Data.Map as M
import qualified Data.Set as S

yearForIdentifier :: (MonadMetadata m, Num a) => Identifier -> m a
yearForIdentifier i = do
  time <- getItemUTC defaultTimeLocale i
  let (year, _, _) = toGregorian . localDay $ utcToLocalTime utc time
  return $ fromIntegral year

archiveGroup :: MonadMetadata m => [Identifier] -> m [[Identifier]]
archiveGroup ids = do
  sorted <- mapM yearMap =<< sortRecentFirst ids
  return $ (fmap snd) <$> groupBy (\a b -> fst a == fst b) sorted
  where
    yearMap :: MonadMetadata m => Identifier -> m (Integer, Identifier)
    yearMap i = yearForIdentifier i >>= \year -> return (year, i)

archiveId :: PageNumber -> Identifier
archiveId pageNum = fromFilePath $ "archive/year/" ++ (show pageNum) ++ "/index.html"

archivePages :: MonadMetadata m => [Identifier] -> m PageNumber
archivePages = yearForIdentifier . head

buildArchivePaginateWith :: MonadMetadata m
                         => ([Identifier] -> m [[Identifier]])
                         -> Pattern
                         -> (PageNumber -> Identifier)
                         -> m Paginate
buildArchivePaginateWith grouper pattern makeId = do
  ids      <- getMatches pattern
  idGroups <- grouper ids
  years    <- mapM archivePages idGroups
  let idsSet = S.fromList ids
  return Paginate
    { paginateMap        = M.fromList $ zip years idGroups
    , paginateMakeId     = makeId
    , paginateDependency = PatternDependency pattern idsSet
    }

