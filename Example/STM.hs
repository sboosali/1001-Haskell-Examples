module Example.STM where


--

import "base"  Control.Monad
import "async" Control.Concurrent
import "stm"   Control.Concurrent.STM


--

newtype Account = Account 
  { getAccount :: 
      TVar Double
  }

--

newAccount :: Double -> STM Account
newAccount balance = Account (newTVar balance)

--

transfer :: Account -> Account -> Double -> STM ()
transfer (Account from) (Account into) amount = do

  available <- readTVar from
  when (amount > available) retry

  modifyTVar from (+ (-amount))
  modifyTVar into  (+ amount)


--
-- Threads are scheduled non-deterministically.


actions :: Account -> Account -> [IO ThreadId]


actions a b = map forkIO

     -- transfer to
     [ atomically (transfer a b 10)
     , atomically (transfer a b (-20))
     , atomically (transfer a b 30)

     -- transfer back
     , atomically (transfer a b (-30))
     , atomically (transfer a b 20)
     , atomically (transfer a b (-10))
   ]


--

main :: IO ()
main = do

  accountA <- atomically $ newAccount 60
  accountB <- atomically $ newAccount 0

  sequence_ (actions accountA accountB)

  balanceA <- atomically $ readTVar (getAccount accountA)
  balanceB <- atomically $ readTVar (getAccount accountB)

  print $ balanceA == 60
  print $ balanceB == 0

--
