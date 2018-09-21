port module Main exposing (..)

import Array exposing (..)
import Types exposing (..)
--import Dict
import Set as Set
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..) 
import Navigation exposing (..)
import UrlParser exposing (Parser, (</>), int, oneOf, s, string)
--import Http exposing (..)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Maybe exposing (withDefault)
import Task
import Window as Win 
import Time exposing (Time, second, now)
import Task exposing (attempt, perform, succeed)
import Random exposing (..)

main =
    Navigation.program urlParser
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

gs = 30

init : Location -> (Model, Cmd Msg)
init loc =
  Model Home
        Nothing
        Nothing
        ""
        ""
        Set.empty
        gs
        Nothing
        --(randomize)
        (glider gs)
        0
        "40"
        "40" ! [newUrl (.hash loc), setSeed] 


setSeed =
    attempt 
      (\t -> 
             case t of 
                Err _  -> SetSeed Nothing 
                Ok t2  -> SetSeed (Just (round t2)))
      now

--UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of 
    NewUrl s -> model ! [newUrl s]

    ChangePos newPos ->  
      if newPos == "#home"
      then { model | currentPos = Home } ! []
      else if newPos == "#login"
      then { model | currentPos = Login } ! []
      else { model | currentPos = Home } ! []

    Loaded s -> 
      {model | loading = Set.remove s (.loading model)} ! []

    Com s -> {model | comChannel = s } ! []

    WinSize s -> 
      { model | winSize = Just s } ! []
    
    SetSeed n ->
      { model | initSeed = n } ! []

    Tick _ ->
      { model | grid = 
                  if .generations model <= 50
                  then nextStep (.grid model) (.gridSize model)
                  else (.grid model)
              , generations = if .generations model <= 50 
                              then (.generations model + 1)
                              else (.generations model)
      } ! []
       
    Random ->
            case .initSeed model of
                Nothing ->
                    { model | error = "No random seed" } ! [setSeed]

                Just sd ->
                    { model
                      |  grid = randomize (.grid model) (.gridSize model) sd 30
                      ,  error = ""
                    } ! [setSeed]

    SetAngleL a -> 
      { model | angleL = a } ! []
    SetAngleR a -> 
      { model | angleR = a } ! []

    Default -> model ! []

--VIEW 
view : Model -> Html Msg
view model = 
  let r = 100
      w = toStr <| 2 * (r + (r / 5)) + (r / 10)
      h = w
      minX = toStr <| -1 * (r + (r / 5)) - (r / 20)
      minY = minX 
      strokeW = "2px"

      nbrFr = 7
      
      dots = extPoints r nbrFr
           |> List.map (\(x,y) -> circle [ cx (toString x)
                                         , cy (toString y)
                                         , stroke "#00396C"
                                         , fill "#00396C"
                                         , Svg.Attributes.r "2"
                                         ]
                                         [])
           |> g []
      
      back = extPoints r nbrFr
            |> pointsToSvg
            |> \pts -> polygon [ pts
                               , stroke "white"
                               , strokeWidth strokeW
                                 --, fill "white"
                               , fill "white"
                               --, Svg.Attributes.mask "url(#mask)"
                               ]
                               [] 

      frame = extPoints r nbrFr
              |> pointsToSvg
              |> \pts -> polygon [ pts
                                 --, stroke "#006867"
                                 --, strokeWidth strokeW
                                 --, fill "white"
                                 , fill "url(#hexaGrid)"
                                 --, Svg.Attributes.mask "url(#mask)"
                                 ]
                                 [] 
      
      frameGrad = extPoints r nbrFr
                |> pointsToSvg
                |> \pts -> polygon [ pts
                                   --, stroke "#00396C"
                                   --, strokeWidth strokeW
                                   --, fill "none"
                                   , fill "url(#grad2)"
                                   , fillOpacity "0.4"
                                   --, Svg.Attributes.mask "url(#mask)"
                                   ]
                                   []
      frameGrad2 = extPoints r nbrFr
                 |> pointsToSvg
                 |> \pts -> polygon [ pts
                                    --, stroke "#006867"
                                    --, strokeWidth strokeW
                                    --, fill "none"
                                    , fill "url(#grad1)"
                                    , fillOpacity "0.7"
                                    --, Svg.Attributes.mask "url(#mask)"
                                    ]
                                    []

      frameHole = extPoints r nbrFr
                  |> pointsToSvg
                  |> \pts -> polygon [ pts
                                     , fill "white"
                                     ]
                                     []
      
      testLine = line [ x1 "0"
                      , y1 "89"
                      , x2 "0"
                      , y2 "62"
                      , strokeWidth "1px"
                      , stroke "white"
                      , strokeDasharray "27"
                      --, strokeDashoffset "1000"
                      ]
                      [animate [ attributeName "stroke-dashoffset"
                               , from "27"
                               , to "0"
                               , dur "0.5s"
                               , fill "freeze"
                               ] []]


      def = defs []
                 [ Svg.mask [ Svg.Attributes.id "mask" 
                        ]
                        [ rect [ Svg.Attributes.width "100%"
                               , Svg.Attributes.height "100%"
                               , x <| toString (-1* ((r+ 40) + 20))
                               , y <| toString (-1* ((r+ 40) + 20))
                               , fill "black"
                               ] []
                        , frameHole
                        ]
                 
                 , Svg.pattern [ Svg.Attributes.id "hexaGrid"
                               , Svg.Attributes.width "100%"
                               , Svg.Attributes.height "100%"
                               , patternContentUnits "objectBoundingBox"
                               ]
                               [Svg.image [xlinkHref "./images/cpuSD.jpg"
                                          , Svg.Attributes.width "1"
                                          , Svg.Attributes.height "1"
                                          , preserveAspectRatio "none"
                                          ]
                                          []
                               ]
                , linearGradient 
                      [ Svg.Attributes.id "grad1"
                      , x1 "0%"
                      , y1 "100%"
                      , x2 "100%"
                      , y2 "0%"

                      ]
                      [ stop [ offset "0.2"
                             , stopColor "#10AEE2"
                             ] --[] 
                             [animate [ attributeName "stop-Color"
                                        , from "#10AEE2"
                                        , to "black"
                                        , dur "5s"
                                        , fill "freeze"
                                        ] []
                               ]
                      , stop [ offset "0.8"
                             , stopColor "black"
                             ] --[]
                             [animate [ attributeName "stop-Color"
                                        , from "black"
                                        , to "#10AEE2"
                                        , dur "5s"
                                        , fill "freeze"
                                        ] []
                               ]
                      ]
                 , Svg.filter 
                      [Svg.Attributes.id "softGlow"]
                      []

                 
                 , radialGradient
                     [ Svg.Attributes.id "grad2"
                
                     , Svg.Attributes.r "50%"
                     , (toFloat <| .generations model) 
                       |> \gen -> ((0.3*gen) / gs) * 100
                       |> round
                       |> toString
                       |> \v -> v ++ "%" 
                       |> cx

                     , (toFloat <| .generations model) 
                       |> \gen -> ((20 - 0.3*gen) / gs) * 100
                       |> round
                       |> toString
                       |> \v -> v ++ "%"
                       |> cy
                     , (toFloat <| .generations model) 
                       |> \gen -> ((0.3*gen) / gs) * 100
                       |> round
                       |> toString
                       |> \v -> v ++ "%" 
                       |> fx

                     , (toFloat <| .generations model) 
                       |> \gen -> ((20 - 0.3*gen) / gs) * 100
                       |> round
                       |> toString
                       |> \v -> v ++ "%"
                       |> fy

                     ]
                     [ stop [ offset "0"
                            , stopColor "#10AEE2"
                            ]
                            []
                     , stop [ offset "1"
                            , stopColor "black"
                            ]
                            []
                     ]
                 ]

  in
  div [ Html.Attributes.style [("background-color","#282425")]
      , Html.Attributes.id "page"
      ]
      [ div [Html.Attributes.id "logo"]
            [ svg [ Svg.Attributes.width "1000px"  
                  , viewBox <| minX ++ " " ++ minY 
                   ++ " " ++ w ++ " " ++ h
                  ]
                  ( [ def
                    --, back
                    , frame
                    , frameGrad2
                    , frameGrad
                    --, dots
                    , testLine
                    , fernLines (.angleL model) (.angleR model)
                    --, fernLinesR (.angleL model) (.angleR model) (Maybe.withDefault 0 (.initSeed model ))
                    ]
                  ++ renderGrid model r 7
                  )
            ]
      --, img [src "/images/Hexacol.png"] []
      --, br [] []
      --, input
      --    [ Html.Attributes.type_ "range"
      --    , Html.Attributes.min "-360"
      --    , Html.Attributes.max "360"
      --    , Html.Attributes.value (.angleL model)
      --    , onInput SetAngleL
      --    , Html.Attributes.style
      --      [ ( "width", "100px" )
      --      , ( "vertical-align", "bottom" )
      --      , ( "outline", "none" )
      --      ]
      --    ]
      --    []
      --, span []
      --       [Html.text (.angleL model)]
      
      --, br [] []
      --, input
      --    [ Html.Attributes.type_ "range"
      --    , Html.Attributes.min "-360"
      --    , Html.Attributes.max "360"
      --    , Html.Attributes.value (.angleR model)
      --    , onInput SetAngleR
      --    , Html.Attributes.style
      --      [ ( "width", "100px" )
      --      , ( "vertical-align", "bottom" )
      --      , ( "outline", "none" )
      --      ]
      --    ]
      --    []
      --, span []
      --       [Html.text (.angleR model)]
      ]

renderGrid : Model -> Int -> Int -> List (Html Msg)
renderGrid model r s =
    let
        gs =
            .gridSize model

        renderCell n cell =
            let
                i =
                    (n // gs * s) - r

                j =
                    (n % gs * s) - r

                (col,sc,sw) =
                    case cell of
                        Dead ->
                            ("none","none","0")

                        Alive ->
                            ("white","#10AEE2","1px")

            in
                rect
                    [ x (toString j)
                    , y (toString i)
                    , Svg.Attributes.width (toString s)
                    , Svg.Attributes.height (toString s)
                    , fill col
                    , strokeWidth sw
                    , stroke sc
                    --, onClick (Click n)
                    , Svg.Attributes.class (case cell of
                                              Dead ->
                                                ""
                                              Alive ->
                                                "cell"
                                            )
                    ]
                    []
    in
        [g [Svg.Attributes.mask "url(#mask)"] (toList (indexedMap renderCell (.grid model)))
        ]
extPoints : Float -> Int -> List (Point)
extPoints r n = 
  let n_ = toFloat n
      angle = 2*pi / n_
      
      go acc n = 
        if n == 0 
        then List.map fromPolar acc
        else go ((r,n*angle - (pi/2)) :: acc) (n-1) 

  in go [] n

pointsToSvg : List Point -> Svg.Attribute msg
pointsToSvg xs = 
  let ptToString (x,y) = 
      let x_ = toString << round <| x
          y_ = toString << round <| y
      in x_ ++ " " ++ y_
      
      ptsStr = String.join " " (List.map ptToString xs)
  
  in points ptsStr  

angFloat angle = 
  String.toInt angle
  |> Result.map toFloat
  |> Result.withDefault 0

rotate : List Point -> String -> List Point 
rotate pts angle = 
  let angle_ = angFloat angle 

      fun p = let (r,tet) = toPolar p 
              in fromPolar (r,tet + degrees angle_)
  in List.map fun pts   

toStr = toString << round


nextStep : Array Cell -> Int -> Array Cell
nextStep grid gs =
    let
        nextCell n cell =
            let
                nb =
                    countNeighbours n
            in
                case cell of
                    Alive ->
                        if (nb == 2 || nb == 3) then
                            Alive
                        else
                            Dead

                    Dead ->
                        if nb == 3 then
                            Alive
                        else
                            Dead

        countNeighbours n =
            let
                i =
                    n // gs

                j =
                    n % gs

                upLeft =
                    withDefault Dead (get (((i - 1) % gs) * gs + ((j - 1) % gs)) grid)

                up =
                    withDefault Dead (get (((i - 1) % gs) * gs + (j % gs)) grid)

                upRight =
                    withDefault Dead (get (((i - 1) % gs) * gs + ((j + 1) % gs)) grid)

                left =
                    withDefault Dead (get ((i % gs) * gs + ((j - 1) % gs)) grid)

                right =
                    withDefault Dead (get ((i % gs) * gs + ((j + 1) % gs)) grid)

                downLeft =
                    withDefault Dead (get (((i + 1) % gs) * gs + ((j - 1) % gs)) grid)

                down =
                    withDefault Dead (get (((i + 1) % gs) * gs + (j % gs)) grid)

                downRight =
                    withDefault Dead (get (((i + 1) % gs) * gs + ((j + 1) % gs)) grid)

                count =
                    List.foldr
                        (\c acc ->
                            if c == Alive then
                                acc + 1
                            else
                                acc
                        )
                        0
                        [ upLeft
                        , up
                        , upRight
                        , left
                        , right
                        , downLeft
                        , down
                        , downRight
                        ]
            in
                count
    in
        indexedMap nextCell grid


randomize : Array Cell -> Int -> Int -> Int -> Array Cell
randomize grid gs sd n =
    let
        indexes =
            makeRange (gs * gs)

        scaledN =
            round <| (toFloat <| gs * gs) * (toFloat n / 100)

        randIndexes =
            List.take scaledN <| shuffle indexes sd

        newGrid =
            initialize (gs * gs) (always Dead)
    in
        List.foldr (\p acc -> Array.set p Alive acc) newGrid randIndexes

glider gs = 
  let grid = repeat (gs * gs) Dead
      gliderIndexes = [450,451,452,482,511]
  in
    List.foldr (\p acc -> Array.set p Alive acc) grid gliderIndexes

shuffle : List a -> Int -> List a
shuffle xs seed =
    let
        l =
            List.length xs

        g =
            Random.int 0 (10 * l)

        indexesGen =
            Random.list l g

        indexList =
            Tuple.first (Random.step indexesGen (initialSeed seed))

        sortedList =
            List.sortWith (\( i1, _ ) ( i2, _ ) -> compare i1 i2) (zip indexList xs)
    in
        List.map Tuple.second sortedList


makeRange : Int -> List Int
makeRange n =
    let
        helper acc n =
            if n == 0 then
                (n :: acc)
            else
                helper (n :: acc) (n - 1)
    in
        helper [] (n - 1)

zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xs_, y :: ys_ ) ->
            ( x, y ) :: zip xs_ ys_

        ( _, _ ) ->
            []

toCmd c =
  Task.perform (\_ -> c) (Task.succeed "")


fern n (r,t) = 
  if n == 0 then [fromPolar(r,t)] 
  else
   let (ox,oy) = fromPolar (r,t)
       (lx,ly) = fromPolar (r/2, 2.09)
       (rx,ry) = fromPolar (r/2, 0.39)
       left  = (lx + ox, ly + oy)
       right = (rx + ox, ry + oy)
   
   in (ox,oy) :: ((fern (n-1) (toPolar left)) ++ (fern (n-1) (toPolar right)))  

fern2 n (r,t) al ar = 
  if n == 0 then [] 
  else
   let (al_,ar_) = ( Result.map degrees (String.toFloat al)
                     |> Result.withDefault 0
                   , Result.map degrees (String.toFloat ar)
                     |> Result.withDefault 0
                   )
       (ox,oy) = fromPolar (r,t)
       (lx,ly) = fromPolar (r/(2*(9-n)), t  + al_)
       (rx,ry) = fromPolar (r/(2*(9-n)), t  - ar_)
       (lx_,ly_) = (lx + ox, ly + oy)
       (rx_,ry_) = (rx + ox, ry + oy)
   
       lineLeft = line [ x1 (toString ox)
                       , y1 (toString (112 - oy))
                       , x2 (toString lx_)
                       , y2 (toString (112 - ly_))
                       , stroke "white"
                       , strokeWidth "1px"
                       , strokeDasharray <| toString (r/(2*(9-n)))
                       , strokeDashoffset <| toString (r/(2*(9-n)))
                       ]
                       [animate [ attributeName "stroke-dashoffset"
                               , from <| toString (r/(2*(9-n)))
                               , to "0"
                               , begin <| toString ((9-n)/2) ++ "s"
                               , dur "0.5s"
                               , fill "freeze"
                               ] []]
       lineRight = line [ x1 (toString ox)
                        , y1 (toString (112 - oy))
                        , x2 (toString rx_)
                        , y2 (toString (112 - ry_))
                        , stroke "white"
                        , strokeWidth "1px"
                        , strokeDasharray <| toString (r/(2*(9-n)))
                        , strokeDashoffset <| toString (r/(2*(9-n)))
                        ]
                        [animate [ attributeName "stroke-dashoffset"
                               , from <| toString (r/(2*(9-n)))
                               , to "0"
                               , begin <| toString ((9-n)/2) ++ "s"
                               , dur "0.5s"
                               , fill "freeze"
                               ] []]

   in lineLeft ::
      lineRight ::
      ((fern2 (n-1) (toPolar (lx_,ly_)) al ar) ++
       (fern2 (n-1) (toPolar (rx_,ry_)) al ar)
      )

fernR n (r,t) al ar seed = 
  if n == 0 then [] 
  else
   let (al_,ar_) = ( Result.map degrees (String.toFloat al)
                     |> Result.withDefault 0
                   , Result.map degrees (String.toFloat ar)
                     |> Result.withDefault 0
                   )
       (ox,oy) = fromPolar (r,t)
       (lx,ly) = fromPolar (r/(2*(9-n)), t  + al_)
       (rx,ry) = fromPolar (r/(2*(9-n)), t  - ar_)
       (lx_,ly_) = (lx + ox, ly + oy)
       (rx_,ry_) = (rx + ox, ry + oy)
       (offset1, nextSeed1) = Random.step (Random.float 0 0.5) seed
       (offset2, nextSeed2) = Random.step (Random.float 0 0.5) nextSeed1
       (_, nextSeed3) = Random.step (Random.float 0 0.5) nextSeed2
   
       lineLeft = line [ x1 (toString ox)
                       , y1 (toString (112 - oy))
                       , x2 (toString lx_)
                       , y2 (toString (112 - ly_))
                       , stroke "white"
                       , strokeWidth "1px"
                       , strokeDasharray <| toString (r/(2*(9-n)))
                       , strokeDashoffset <| toString (r/(2*(9-n)))
                       ]
                       [animate [ attributeName "stroke-dashoffset"
                               , from <| toString (r/(2*(9-n)))
                               , to "0"
                               , begin <| toString (offset1  + (9-n)/2) ++ "s"
                               , dur <| toString (0.5 - offset1) ++ "s"
                               , fill "freeze"
                               ] []]
       lineRight = line [ x1 (toString ox)
                        , y1 (toString (112 - oy))
                        , x2 (toString rx_)
                        , y2 (toString (112 - ry_))
                        , stroke "white"
                        , strokeWidth "1px"
                        , strokeDasharray <| toString (r/(2*(9-n)))
                        , strokeDashoffset <| toString (r/(2*(9-n)))
                        ]
                        [animate [ attributeName "stroke-dashoffset"
                               , from <| toString (r/(2*(9-n)))
                               , to "0"
                               , begin <| toString (offset2 + (9-n)/2) ++ "s"
                               , dur <| toString (0.5 - offset2) ++ "s"
                               , fill "freeze"
                               ] []]

   in lineLeft ::
      lineRight ::
      ((fernR (n-1) (toPolar (lx_,ly_)) al ar nextSeed2) ++
       (fernR (n-1) (toPolar (rx_,ry_)) al ar nextSeed3)
      )

fernLines al ar = 
  g [] (fern2 8 (toPolar (0,50)) al ar)

fernLinesR al ar seed = 
  g [] (fernR 8 (toPolar (0,50)) al ar (initialSeed seed))

fernPoints = 
  let pts = fern 8 (toPolar (0,10))
  in g [] 
       (List.map (\(x,y) -> circle [ cx (toString x)
                                   , cy (toString y)
                                   , stroke "white"
                                   , r "1"
                                   ]
                                   []) pts
       ) 

-- NAVIGATION
urlParser : (Location -> Msg)
urlParser loc = ChangePos (.hash loc)
  

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Win.resizes WinSize
              , Time.every (0.085 * second) Tick
              ]