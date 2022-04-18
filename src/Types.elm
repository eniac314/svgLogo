module Types exposing (..)

import Window as Win
import Http exposing (Error)
import Dict exposing (..)
import Set exposing (..)
import Date exposing (..)
import Array exposing (..)
import Time exposing (Time, second, now)



type alias Model = 
  { currentPos : Position
  , winSize    : Maybe Win.Size
  , loginInfo  : Maybe LogInfo
  , comChannel : String 
  , error      : String
  , loading    : Set String
  , gridSize : Int
  , initSeed : Maybe Int
  , grid : Array Cell
  , generations : Int
  , angleL : String
  , angleR : String
  }

type Cell
    = Alive
    | Dead

type Position = Home 
              | Login 


type alias LogInfo = 
  { username : String
  , formToken : String
  , sessionId : String
  }

type Msg 
     = ChangePos String
     | WinSize Win.Size
     | Loaded String
     | Com String
     | NewUrl String
     | SetSeed (Maybe Int)
     | Tick Time
     | Default
     | Random
     | SetAngleL String
     | SetAngleR String

type alias Point = (Float,Float)