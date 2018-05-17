module Main where

import Control.Monad.Ref (Ref)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.Either.Utils (maybeToEither)
import Data.String.Utils (maybeRead)
import Data.Word (Word16)
import Reflex
import Reflex.Host.Class


type Id = Either String Word16
data Node = Const Id
          | Not Id
          | And Id Id
          | Or Id Id
          | LShift Id Id
          | RShift Id Id

type NodeMapVal t m = (Behavior t Word16, Ref m (Maybe (EventTrigger t (Behavior t Word16))))
type NodeMap t m = HashMap String (NodeMapVal t m)

parseNode :: String -> (String, Node)
parseNode line = case words line of
                   [             a, "->", v] -> (v, Const $ f a)
                   [   "NOT",    a, "->", v] -> (v, Not $ f a)
                   [a, "AND",    b, "->", v] -> (v, f a `And` f b)
                   [a, "OR",     b, "->", v] -> (v, f a `Or` f b)
                   [a, "LSHIFT", b, "->", v] -> (v, f a `LShift` f b)
                   [a, "RSHIFT", b, "->", v] -> (v, f a `RShift` f b)
    where f :: String -> Id
          f x = maybeToEither x $ maybeRead x

buildWires :: [String] -> SpiderHost (NodeMap Spider SpiderHost)
buildWires = addWires M.empty
    where addWires m [] = return m
          addWires m (x:xs) = do
            let (w, node) = parseNode x
            nodeb <- runMaybeT $ eval m node
            case nodeb of
              Just v  -> addWires (M.insert w v m) xs
              Nothing -> addWires m (xs ++ [x])

eval :: NodeMap Spider SpiderHost -> Node -> MaybeT SpiderHost (NodeMapVal Spider SpiderHost)
eval m node =
    case node of
      (Const nd) -> single nd id
      (Not nd) -> single nd complement
      (And nd1 nd2) -> double nd1 nd2 (.&.)
      (Or nd1 nd2) -> double nd1 nd2 (.|.)
      (LShift nd1 nd2) -> double nd1 nd2 (\a b -> shiftL a (fromIntegral b))
      (RShift nd1 nd2) -> double nd1 nd2 (\a b -> shiftR a (fromIntegral b))
    where getB :: Id -> MaybeT SpiderHost (Behavior Spider Word16)
          getB = MaybeT . either (\x -> return $ fst <$> M.lookup x m)
                                 (\x -> Just <$> (newEventWithTriggerRef >>= hold x . fst))
          single nd f = do
            b <- getB nd
            (event, fire) <- lift newEventWithTriggerRef
            behavior <- lift $ switcher (f <$> b) event
            return (behavior, fire)
          double nd1 nd2 f = do
            b1 <- getB nd1
            b2 <- getB nd2
            (event, fire) <- lift newEventWithTriggerRef
            behavior <- lift $ switcher (f <$> b1 <*> b2) event
            return (behavior, fire)

part1 :: String -> SpiderHost Word16
part1 input = do
  m <- buildWires $ lines input
  sample . fst $ m ! "a"

part2 :: String -> SpiderHost Word16
part2 input = do
  m <- buildWires $ lines input
  v <- sample . fst $ m ! "a"
  fireEventRef (snd $ m ! "b") (pure v)
  sample . fst $ m ! "a"

main :: IO ()
main = do
  inp <- readFile "input2.txt"
  runSpiderHost (part1 inp) >>= print
  runSpiderHost (part2 inp) >>= print
