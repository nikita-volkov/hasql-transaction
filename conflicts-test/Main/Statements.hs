module Main.Statements where

import Rebase.Prelude
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


createAccountTable :: Statement () ()
createAccountTable =
  Statement sql E.unit D.unit False
  where
    sql =
      "create table account (id serial not null, balance numeric not null, primary key (id))"

dropAccountTable :: Statement () ()
dropAccountTable =
  Statement
    "drop table account"
    E.unit
    D.unit
    False

createAccount :: Statement Scientific Int64
createAccount =
  Statement
    "insert into account (balance) values ($1) returning id"
    (E.param E.numeric)
    (D.singleRow (D.column D.int8))
    True

modifyBalance :: Statement (Int64, Scientific) Bool
modifyBalance =
  Statement
    "update account set balance = balance + $2 where id = $1"
    (contrazip2 (E.param E.int8) (E.param E.numeric))
    (fmap (> 0) D.rowsAffected)
    True

getBalance :: Statement Int64 (Maybe Scientific)
getBalance =
  Statement
    "select balance from account where id = $1"
    (E.param E.int8)
    (D.rowMaybe (D.column D.numeric))
    True

