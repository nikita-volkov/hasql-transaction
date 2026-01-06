module Main.Statements where

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement
import Prelude

createAccountTable :: Statement () ()
createAccountTable =
  unpreparable sql E.noParams D.noResult
  where
    sql =
      "create table account (id bigserial not null, balance numeric not null, primary key (id))"

dropAccountTable :: Statement () ()
dropAccountTable =
  unpreparable
    "drop table account"
    E.noParams
    D.noResult

createAccount :: Statement Scientific Int64
createAccount =
  preparable
    "insert into account (balance) values ($1) returning id"
    ((E.param . E.nonNullable) E.numeric)
    (D.singleRow ((D.column . D.nonNullable) D.int8))

modifyBalance :: Statement (Int64, Scientific) Bool
modifyBalance =
  preparable
    "update account set balance = balance + $2 where id = $1"
    ((fst >$< (E.param . E.nonNullable) E.int8) <> (snd >$< (E.param . E.nonNullable) E.numeric))
    (fmap (> 0) D.rowsAffected)

getBalance :: Statement Int64 (Maybe Scientific)
getBalance =
  preparable
    "select balance from account where id = $1"
    ((E.param . E.nonNullable) E.int8)
    (D.rowMaybe ((D.column . D.nonNullable) D.numeric))
