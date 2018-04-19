module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (AddressBook, Entry, emptyBook, insertEntry, findEntry, showEntry, findEntryByStreet, isEntryInBook)
import Data.Foldable (foldl)
import Data.Maybe (Maybe)

example :: Array Entry
example =
  [
    { firstName: "John"
    , lastName: "Smith"
    , address: { street: "123 Fake St."
              , city: "Faketown"
              , state: "CA"
              }
    },
    { firstName: "Jane"
    , lastName: "Smith"
    , address: { street: "123 Fake St."
              , city: "Faketown"
              , state: "CA"
              }
    },
    { firstName: "David"
    , lastName: "Anderson"
    , address: { street: "321 Fake St."
              , city: "Fakecity"
              , state: "NY"
              }
    }
  ]

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

main :: Eff (console :: CONSOLE) Unit
main = do
  let book1 = foldl (\acc e -> insertEntry e acc) emptyBook example

  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1
  logShow $ printEntry "Jane" "Smith" book1

  logShow $ showEntry <$> findEntryByStreet "321 Fake St." book1

  logShow $ isEntryInBook "Jane" "Smith" book1
  logShow $ isEntryInBook "Jonathan" "Smith" book1
