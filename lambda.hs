import qualified Prelude as P

infixl 6 <$>

data F =
  F { (<$>) :: F -> F }
  | N { unN :: P.Integer }

i = F (\x -> x)
k = F (\c -> F (\_ -> c))
s = F (\f -> F (\g -> F (\x -> (f <$> x) <$> (g <$> x))))

zero = k <$> i
succ = F (\n -> F (\f -> F (\x -> f <$> (n <$> f <$> x))))
add = F (\m -> m <$> succ)
mult = F (\m -> F (\n -> m <$> (add <$> n) <$> zero))
pow = F (\m -> F (\n -> n <$> m))

one = succ <$> zero
two = succ <$> one
three = succ <$> two
four = add <$> two <$> two
six = mult <$> three <$> two
eight = mult <$> four <$> two
n16 = pow <$> two <$> four
n24 = mult <$> six <$> four
n32 = mult <$> eight <$> four
n_large = pow <$> two <$> n16

churchToInt n = unN (n <$> F (\(N x) -> N (P.succ x)) <$> N 0)

main = P.print (churchToInt n_large)
