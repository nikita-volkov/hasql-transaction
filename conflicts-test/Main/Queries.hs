module Main.Queries where

import Rebase.Prelude
import Hasql.Query
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


createAccountTable :: Query () ()
createAccountTable =
  statement sql E.unit D.unit False
  where
    sql =
      "create table account (id serial not null, balance numeric not null, primary key (id))"

dropAccountTable :: Query () ()
dropAccountTable =
  statement
    "drop table account"
    E.unit
    D.unit
    False

createAccount :: Query Scientific Int64
createAccount =
  statement
    "insert into account (balance) values ($1) returning id"
    (E.value E.numeric)
    (D.singleRow (D.value D.int8))
    True

modifyBalance :: Query (Int64, Scientific) Bool
modifyBalance =
  statement
    "update account set balance = balance + $2 where id = $1"
    (contrazip2 (E.value E.int8) (E.value E.numeric))
    (fmap (> 0) D.rowsAffected)
    True

getBalance :: Query Int64 (Maybe Scientific)
getBalance =
  statement
    "select balance from account where id = $1"
    (E.value E.int8)
    (D.maybeRow (D.value D.numeric))
    True

