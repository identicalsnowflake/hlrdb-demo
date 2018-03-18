module Main where

import Data.Store
import GHC.Generics
import Data.Profunctor.Traversing
import Data.Monoid
import Database.Redis (Connection,checkedConnect,defaultConnectInfo,runRedis)
import Data.Maybe (mapMaybe)
import Data.Foldable
import Data.Traversable
import Control.Monad.State

import HLRDB


-- example data model
newtype CommentId = CommentId Identifier deriving (Eq,Ord,Show,Store,IsIdentifier)

newtype ThreadId = ThreadId Identifier deriving (Eq,Ord,Show,Store,IsIdentifier)

newtype Comment = Comment String deriving (Eq,Ord,Show,Store)


-- A path from ThreadId to a Redis list of CommentIds
tidToComments :: RedisList ThreadId CommentId
tidToComments = declareList "ThreadId ~> [ CommentId ]" $ Just (1000 , 0.05)

-- A standard key-value mapping in Redis
cidToComment :: RedisBasic CommentId (Maybe Comment)
cidToComment = declareBasic "canonical mapping from CommentId to Comment"

-- Views and Likes are mappings to integers in Redis. RedisIntegral is a
-- subtype of the standard key-value store which has additional
-- primitive commands available, like incr and decr.

newtype ViewCount = ViewCount Integer deriving (Show,Eq,Ord,Num,Enum,Real,Integral)
newtype LikeCount = LikeCount Integer deriving (Show,Eq,Ord,Num,Enum,Real,Integral)


cidToViewCount :: RedisIntegral CommentId ViewCount
cidToViewCount = declareIntegral "comment views"

cidToLikeCount :: RedisIntegral CommentId LikeCount
cidToLikeCount = declareIntegral "comment likes"


-- example data view typically given to API client users via JSON

data CommentView = CommentView {
    views :: !ViewCount
  , likes :: !LikeCount
  , commentId :: !CommentId
  , comment :: !Comment
  } deriving (Generic,Show)


-- Our basic path declarations can be lifted to queries with liftq,
-- which have several combinators available, like <$>, <*>, and remember.
getCommentView :: CommentId ⟿ Maybe CommentView
getCommentView =
  optq (liftq cidToComment)
     $  CommentView
    <$> liftq cidToViewCount
    <*> liftq cidToLikeCount

  where
    optq :: i ⟿ Maybe a -> i ⟿ (i -> a -> b) -> i ⟿ Maybe b
    optq q p = rememberAp $ flip ((.) . flip fmap) <$> p <*> q
      where
        rememberAp :: a ⟿ (a -> b) -> a ⟿ b
        rememberAp = fmap (\(x,f) -> f x) . remember


exampleUsage :: Connection -> IO ()
exampleUsage conn = do
  
  tid :: ThreadId <- genId

  -- create some comments and add them to the thread
  _ <- do

    cids :: [ CommentId ] <- traverse (const genId) [ 1 .. 10 :: Int ]

    runRedis conn $ do

      -- create a comment for each cid
      for_ cids $ \cid -> set' cidToComment cid .
        Comment $ "Example comment (" <> show cid <> ")"

      -- add the cids to our comment thread
      lprepend tidToComments tid cids

      -- add some likes and views
      flip evalStateT 1 $ for cids $ \cid -> do
        
        i <- state $ \s -> (s , s + 1)

        lift $ do
          incrby cidToViewCount cid (fromInteger i + 10)
          incrby cidToLikeCount cid (fromInteger i)


  -- now let's read the some of the data back from Redis with our view
  commentViews :: [ CommentView ] <- runRedis conn $ do
    
     -- retrieve the first 5 items
    cids :: [ CommentId ] <-
      lrange tidToComments tid 0 5


    -- render the views
    mviews :: [ Maybe CommentView ] <- do
      
      -- make sure to traverse' ⟿ directly, so we get a single large, efficient query
      let efficientQuery :: [ CommentId ] ⟿ [ Maybe CommentView ]
                         =  traverse' getCommentView
      
      mget efficientQuery cids

    pure $ mapMaybe id mviews

  -- and print the results
  for_ commentViews $ print

main :: IO ()
main = checkedConnect defaultConnectInfo >>= exampleUsage

