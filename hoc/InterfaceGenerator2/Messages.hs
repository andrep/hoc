module Messages(Messages, message, runMessages, listenMessages) where

import Control.Monad.Writer
import Text.PrettyPrint.HughesPJ

{-instance Monoid Doc where
    mempty = PP.empty
    mappend = ($+$)
    mconcat = vcat-}

data Bag a = EmptyBag | UnitBag a | BagOfTwo (Bag a) (Bag a)

-- invariant: first sub-bag of a BagOfTwo is non-empty

bagToList b = helper b []
    where
        helper EmptyBag xs = xs
        helper (UnitBag x) xs = x : xs
        helper (BagOfTwo a b) xs = helper a $ helper b xs 

instance Monoid (Bag a) where
    mempty = EmptyBag
    mappend EmptyBag b = b
--    mappend a EmptyBag = a    more lazyness!
    mappend a b = BagOfTwo a b

type Messages = Writer (Bag Doc)
message d = tell (UnitBag d)

runMessages :: Messages a -> (a, [Doc])
listenMessages :: Messages a -> Messages (a, [Doc])

runMessages msg = case runWriter msg of
    (a, w) -> (a, bagToList w)
           
listenMessages msg
    = do
        (a, w) <- listen msg
        return (a, bagToList w)
