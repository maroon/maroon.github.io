module Archive
  ( archiveContext
  , archiveGroup
  , archiveId
  , buildArchivePaginateWith
  , yearForIdentifier
  ) where

import Control.Applicative (empty)
import Control.Monad (forM)
import Data.Bool (bool)
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

archiveContext :: Paginate -> PageNumber -> Context a
archiveContext pag currentPage = mconcat
  [ field "firstPageNum"    $ \_ -> otherPage lastPage     >>= num
  , field "firstPageUrl"    $ \_ -> otherPage lastPage     >>= url
  , field "previousPageNum" $ \_ -> otherPage previousPage >>= num
  , field "previousPageUrl" $ \_ -> otherPage previousPage >>= url
  , field "nextPageNum"     $ \_ -> otherPage nextPage     >>= num
  , field "nextPageUrl"     $ \_ -> otherPage nextPage     >>= url
  , field "lastPageNum"     $ \_ -> otherPage firstPage    >>= num
  , field "lastPageUrl"     $ \_ -> otherPage firstPage    >>= url
  , field "currentPageNum"  $ \i -> thisPage i             >>= num
  , field "currentPageUrl"  $ \i -> thisPage i             >>= url
  , constField "numPages"   $ show lastPage
  , Context $ \k _ i ->
      case k of
        "allPages" -> do
          let ctx =
                field "isCurrent" (\n -> bool empty (return "true") $ fst (itemBody n) == currentPage) <>
                field "num" (num . itemBody) <>
                field "url" (url . itemBody)

          list <- forM [1..lastPage] $
            \n -> bool (otherPage n) (thisPage i) (n == currentPage)
          items <- mapM makeItem list
          return $ ListField ctx items
        _          -> empty
  ]
  where
    firstPage :: PageNumber
    firstPage = head . M.keys $ paginateMap pag

    lastPage :: PageNumber
    lastPage = last . M.keys $ paginateMap pag

    previousPage :: PageNumber
    previousPage = do
      let pages = M.keys . snd $ M.split currentPage (paginateMap pag)
      bool (head pages) 0 $ null pages

    nextPage :: PageNumber
    nextPage = do
      let pages = M.keys . fst $ M.split currentPage (paginateMap pag)
      bool (last pages) 0 $ null pages

    thisPage :: Item a -> Compiler (Int, Identifier)
    thisPage i = return (currentPage, itemIdentifier i)

    otherPage :: PageNumber -> Compiler (Int, Identifier)
    otherPage n
      | n == currentPage = fail $ "This is the current page: " ++ show n
      | otherwise        =
        case M.member n (paginateMap pag) of
          True  -> return (n, paginateMakeId pag n)
          False -> fail $ "No such page: " ++ show n

    num :: (Int, Identifier) -> Compiler String
    num = return . show . fst

    url :: (Int, Identifier) -> Compiler String
    url (n, i) = getRoute i >>= \mbR -> case mbR of
      Just r  -> return $ toUrl r
      Nothing -> fail $ "No URL for page: " ++ show n
