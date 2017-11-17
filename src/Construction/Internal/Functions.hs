{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here,
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta, eq
  )where


import           Construction.Internal.Types (Name, Term (..))
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
import           Data.Text                   (pack)


-- Context is just set of names that are in our context.
type Context = Set Name

-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]

-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free Var{..} = singleton var
free App{..} = free algo `union` free arg
free Lam{..} = variable `delete` free body

-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body


-- a[n := b] - substitution
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n  = b
                         | otherwise = v
substitute App{..} n b                                  = App (substitute algo n b) (substitute arg n b)
substitute lam@Lam{..} n b | variable == n              = lam
                           | variable `member` (free b) = substitute (alpha lam (free b)) n b
                           | otherwise                  = Lam variable (substitute body n b)

alpha :: Term -> Set Name -> Term
alpha v@Var{..} nameSet                             = v 
alpha App{..} nameSet                               = App (alpha algo nameSet) (alpha arg nameSet)
alpha l@Lam{..} nameSet | variable `member` nameSet = let newVar = fresh (nameSet `union` (free body)) 
                                                      in Lam newVar (alpha (substitute body variable (Var newVar)) nameSet)
                        | otherwise                 = Lam variable (alpha body (variable `insert` nameSet))

beta :: Term -> Term
beta (App Lam{..} arg) = substitute (beta body) variable (beta arg)
beta App{..}           = App (beta algo) (beta arg)
beta Lam{..}           = Lam variable (beta body)
beta v@Var{..}         = v


eta :: Term -> Term
eta lam@(Lam var (App algo (Var variable))) | variable `member` (free algo) = lam 
                                            | variable == var               = algo
                                            | otherwise                     = lam 
eta t = t

-- | eq
eq :: Term -> Term -> Bool
eq t1 t2 = helper (reduce t1) (reduce t2)
  where helper v1@Var{..} v2@Var{}               = v1 == v2
        helper (App algo1 arg1) (App algo2 arg2) = (helper algo1 algo2) && (helper arg1 arg2)
        helper (Lam var1 body1) (Lam var2 body2) = let termName = Var (fresh (var2 `insert` (free (body1))))
                                       in helper (substitute body1 var1 termName) (substitute body2 var2 termName)


instance Eq Term where
  Var v1 == Var v2 = v1 == v2
  App algo1 arg1 == App algo2 arg2 = algo1 == algo2 && arg1 == arg2
  Lam v1 b1 == Lam v2 b2 = sub1 == sub2
    where
      freshVar = Var $ fresh $ free b1 `union` free b2
      sub1 = substitute b1 v1 freshVar
      sub2 = substitute b2 v2 freshVar
  _ == _ = False


-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'
