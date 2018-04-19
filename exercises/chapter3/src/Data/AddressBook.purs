module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe, isNothing)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


-- Exercise solutions for chapter 3.

-- Exercise 1.
--  head :: Addressbook -> Maybe Entry
--  filter :: (Entry -> Boolean) -> Addressbook -> Array Entry

-- Exercise 2.
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntryByStreet
  where
  filterEntryByStreet :: Entry -> Boolean
  filterEntryByStreet entry = entry.address.street == street

-- Exercise 3.
isEntryInBook :: String -> String -> AddressBook -> Boolean
isEntryInBook first last = not <<< isNothing <<< findEntry first last

-- Exercise 4.
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy namesMatch
  where
  namesMatch :: Entry -> Entry -> Boolean
  namesMatch a b = a.firstName == b.firstName && a.lastName == b.lastName