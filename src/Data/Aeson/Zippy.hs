{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Data.Aeson.Zippy
  ( Decoder
  , Result(..)
  , JCurs
  , ConversionFailure(..)
  , BoundError(..)
  , JTy(..)
  , Wrapped(..)
  , Message(..)
  , throwCustomError
  , isNull
  , attoparsec
  , int
  , text
  , bool
  , aeson
  , list
  , nonempty
  , atKey  
  , down
  , up
  , moveToKey
  , withCursor
  , fromKeyOptional
  , focus
  , right
  , rightMaybe
  , left
  , leftMaybe
  , foldr
  , parseValue
  , isEmpty
  , root
  , jsonPath
  ) where

import Prelude hiding (foldr, read, fail)

import Data.Typeable
import Data.Functor.Alt
import Data.Functor (($>))
import qualified Data.Aeson.Types as AT
import qualified Data.Attoparsec.Text as APS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Control.Monad.Except
import qualified Data.Scientific as S

data JCurs = Top AT.Value
           | Array JCurs Int (V.Vector AT.Value)
           | Object JCurs T.Text AT.Object

data JTy = JArray
         | JObject
         | JString
         | JNumber
         | JNull
         | JBool
         deriving (Eq, Show)
  
newtype Decoder a = Decoder { runDecoder :: JCurs -> Result a }
data Result a = Success a | Failure JCurs ConversionFailure

data BoundError = TooHigh | IndexError Int deriving (Show)
data ConversionFailure = AesonError T.Text
                       | AttoparsecError T.Text 
                       | TypeMismatch [JTy]      -- | Expected types
                       | FieldMissing T.Text     -- | Name of missing field
                       | OutOfBounds BoundError  -- | Attempt to move outside bounds of array or object ('up' when at the top of json document or accessing outside bounds of array
                       | EmptyContainer          -- | Result of attempting to 'down' into an empty array or object
                       | Other Wrapped           -- | Custom user error produced with 'throwCustomError'
                       deriving Show
data Wrapped = forall r. (Typeable r, Show r) => Wrapped r

instance Show Wrapped where
  show (Wrapped inner) = show inner

data Message = Message T.Text deriving (Typeable, Show)

instance Functor Decoder where
  fmap f (Decoder df) = Decoder $ \curs -> f <$> df curs

instance Alt Decoder where
  fa <!> fb = Decoder $ \curs ->
    case runDecoder fa curs of
      s@(Success _) -> s
      Failure _ _ -> runDecoder fb curs
      
instance Applicative Decoder where
  pure v = Decoder $ \_ -> pure v
  df <*> da = Decoder $ \curs ->
    case runDecoder df curs of
      Success f -> f <$> runDecoder da curs
      Failure curs' fa -> Failure curs' fa

instance Monad Decoder where
  da >>= f = Decoder $ \curs ->
    case runDecoder da curs of
      Success a -> runDecoder (f a) curs
      Failure curs' e -> Failure curs' e

instance MonadError ConversionFailure Decoder where
  throwError e = Decoder $ \curs -> Failure curs e
  catchError ma handler = Decoder $ \curs ->
    case runDecoder ma curs of
      Success v -> Success v
      Failure _ e -> runDecoder (handler e) curs

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure curs fa) = Failure curs fa

instance Applicative Result where
  pure = Success  
  Success f <*> Success a = Success (f a)
  Failure curs fa <*> _ = Failure curs fa
  _ <*> Failure curs da = Failure curs da

instance Monad Result where
  Success a  >>= f = f a
  Failure curs fa >>= _ = Failure curs fa

throwCustomError :: (Typeable r, Show r) => r -> Decoder a
throwCustomError = throwError . Other . Wrapped

jtype :: AT.Value -> JTy
jtype = \case
  AT.Object _ -> JObject
  AT.Array _ -> JArray
  AT.String _ -> JString
  AT.Number _ -> JNumber
  AT.Bool _ -> JBool
  AT.Null -> JNull
  
mkCursor :: AT.Value -> JCurs
mkCursor = Top

focusType :: JCurs -> JTy
focusType = jtype . cursorFocus

cursorFocus :: JCurs -> AT.Value
cursorFocus = \case
  Top f -> f
  Array _ ix arr -> arr V.! ix
  Object _ name obj -> obj HM.! name

value :: Decoder AT.Value
value = withCursor $ pure . cursorFocus

-- | Run 'Decoder' on cursor
focus :: Decoder a -> JCurs -> Result a
focus (Decoder f) cur = f cur

withCursor :: (JCurs -> Result a) -> Decoder a
withCursor = Decoder

fromKeyOptional :: T.Text -> Decoder b -> JCurs -> Result (Maybe b)
fromKeyOptional field dec curs = do
  mcurs <- moveToKeyMaybe field curs
  case mcurs of
    Just curs' -> Just <$> focus dec curs'
    Nothing -> pure Nothing

nonempty :: Decoder a -> Decoder (NE.NonEmpty a)
nonempty d = do
  ls <- list d
  case NE.nonEmpty ls of
    Nothing -> throwCustomError $ Message "non-empty is empty"
    Just v -> pure v

-- | Determines if focused value is not null and if the value is an array or object
-- checks if at least one entry is present
isEmpty :: JCurs -> Bool
isEmpty curs = case cursorFocus curs of
  AT.Array arr -> V.null arr
  AT.Object obj -> HM.null obj
  AT.Null -> True
  _ -> False

-- | Fold over array at focus 
foldr :: (a -> b -> b) -> b -> Decoder a -> Decoder b
foldr f b dec = withCursor $ \curs -> do
  if focusType curs == JArray && isEmpty curs
    then pure b
    else down curs >>= loop
  where
    loop lc = do
      v <- focus dec lc
      attempt <- rightMaybe lc
      case attempt of
        Nothing -> pure (f v b)
        Just lc' -> f v <$> loop lc'

-- | Parse list of 'Decoder'
--
-- See also 'nonempty'
list :: Decoder a -> Decoder [a]
list = foldr (:) []

-- | Focus on key inside object. Fails if not inside object
moveToKeyMaybe :: T.Text -> JCurs -> Result (Maybe JCurs)
moveToKeyMaybe field curs = case curs of
  Object p _ obj -> pure $ HM.lookup field obj $> Object p field obj
  _ -> Failure curs $ TypeMismatch [JObject]

-- | Set cursor focus to field of object
moveToKey :: T.Text -> JCurs -> Result JCurs  
moveToKey field curs = do
  attempt <- moveToKeyMaybe field curs
  case attempt of
    Just curs' -> pure curs'
    Nothing -> Failure curs (FieldMissing field)

-- | Enter JSON array or object
--
-- Empty arrays or objects cannot be entered; array / object emptyness can be
-- tested with 'isEmpty'
--
-- When entering an array the focus is set to the first value; entering objects sets an arbitrary key as the focus
down :: JCurs -> Result JCurs
down p = downTo . cursorFocus $ p
  where
    failWith :: ConversionFailure -> Result a
    failWith reason = Failure p reason
    downToObj obj = case HM.keys obj of
      (x:_) -> pure $ Object p x obj
      [] -> failWith EmptyContainer      
    downToArr arr = case V.null arr of
      False -> pure $ Array p 0 arr
      True -> failWith EmptyContainer
    downTo v = case v of
      AT.Object obj -> downToObj obj
      AT.Array arr -> downToArr arr
      _            -> failWith $ TypeMismatch [JObject, JArray]

up :: JCurs -> Result JCurs
up curs = case curs of
  Top _ ->  Failure curs $ OutOfBounds TooHigh
  Array p _ _ -> pure p
  Object p _ _ -> pure p

cursorIndex :: JCurs -> Result Int
cursorIndex curs = case curs of
  Array _ ix _ -> pure ix
  _            -> Failure curs $ TypeMismatch [JArray]

cursorLength :: JCurs -> Result Int
cursorLength curs = case curs of
  Array _ _ vs -> pure $ V.length vs
  _            -> Failure curs $ TypeMismatch [JArray]
  
moveIxMaybe :: Int -> JCurs -> Result (Maybe JCurs)
moveIxMaybe ix curs = case curs of
  Array p _ arr | 0 <= ix && V.length arr > ix -> pure $ Just (Array p ix arr)
  Array _ _ _                                  -> pure Nothing
  _                                            -> Failure curs $ TypeMismatch [JArray]

right :: JCurs -> Result JCurs
right curs = do
  mcurs <- rightMaybe curs
  case mcurs of
    Just curs' -> pure curs'
    Nothing -> cursorLength curs >>= Failure curs . OutOfBounds . IndexError

left :: JCurs -> Result JCurs
left curs = do
  mcurs <- leftMaybe curs
  case mcurs of
    Just curs' -> pure curs'
    Nothing -> Failure curs $ OutOfBounds $ IndexError (-1)
    
rightMaybe :: JCurs -> Result (Maybe JCurs)
rightMaybe curs = do
  ix <- cursorIndex curs
  moveIxMaybe (ix+1) curs

leftMaybe :: JCurs -> Result (Maybe JCurs)
leftMaybe curs = do
  ix <- cursorIndex curs
  moveIxMaybe (ix-1) curs

atKey :: T.Text -> Decoder a -> Decoder a
atKey field dec =
  withCursor $ down >=> moveToKey field >=> focus dec

int :: Integral i => Decoder i
int = value >>= \case
  AT.Number s ->
    case S.floatingOrInteger s of
      Right i -> pure i
      _       -> throwCustomError $ Message "numeric not integer"
  _  -> throwError $ TypeMismatch [JNumber]

-- | Extract string field from focus
text :: Decoder T.Text
text = value >>= \case
  AT.String s -> pure s
  _           -> throwError $ TypeMismatch [JString]

-- | Extract bool field from focus
bool :: Decoder Bool
bool = value >>= \case
  AT.Bool b -> pure b
  _         -> throwError $ TypeMismatch [JBool]

-- | Construct a 'Decoder' from a 'AT.FromJSON' instance
aeson :: AT.FromJSON a => Decoder a
aeson = do
  v <- value
  case AT.fromJSON v of
    AT.Success pv -> pure pv
    AT.Error msg -> throwError $ AesonError (T.pack msg)

-- | Create a 'Decoder' from a 'APS.Parser' to parse a string field
attoparsec :: APS.Parser a -> Decoder a
attoparsec parser = do
  t <- text
  case APS.parseOnly parser t of
    Right v -> pure v
    Left msg -> throwError $ AttoparsecError (T.pack msg)

parseValue :: Decoder a -> AT.Value -> Result a
parseValue decoder = runDecoder decoder . mkCursor

isNull :: JCurs -> Bool
isNull curs = focusType curs == JNull

-- | Get root 'AT.Value' from 'JCurs'
root :: JCurs -> AT.Value
root = \case
  Top f -> f
  Array p _ _ -> root p
  Object p _ _ -> root p

-- | Render JSON path of 'JCurs'
jsonPath :: JCurs -> T.Text
jsonPath = T.intercalate "." . reverse . fmap render . parts
  where
    render = \case
      Top _ -> "$"
      Array _ ix _ -> "[" <> (T.pack . show $ ix) <> "]"
      Object _ field _ -> field
    parts = \case
      t@(Top _) -> [t]
      t@(Array p _ _) -> t : parts p
      t@(Object p _ _) -> t : parts p
