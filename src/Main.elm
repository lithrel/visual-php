module Main exposing (Model, init, Msg, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (attribute, style, class)
import Browser
import Browser.Events as BrEv
import Json.Decode as Decode exposing (Decoder, int, string, float, list)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import RemoteData exposing (RemoteData, WebData)
import Http
import Svg exposing (Svg, svg, rect, foreignObject)
import Svg.Attributes exposing (height, width, viewBox, x, y, fill, id)
import Svg.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Debug

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Position =
    (Float, Float)

type alias FunctionPosition =
    (String, Position)

type alias Model =
    { functions : WebData (List Function)
    , dragging : Maybe String
    , positions : List FunctionPosition
    , pointerPos : Position
    , pointerPosOnDown : Maybe Position
    , relPointerPosOnDown : Maybe Position
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( 
        { functions = RemoteData.Loading
        , dragging = Nothing 
        , positions = []
        , pointerPos = (0,0)
        , pointerPosOnDown = Nothing
        , relPointerPosOnDown = Nothing
        }
        , fetchFunctions 
    )

fetchFunctions : Cmd Msg
fetchFunctions =
    Http.get
        { url = "http://localhost:9876/api/"
        , expect = 
            list functionDecoder
                |> Http.expectJson (RemoteData.fromResult >> FunctionsReceived)
        }

type Msg
    = FetchFunctions
    | FunctionsReceived (WebData (List Function))
    | PointerDownMsg  String Mouse.Event
    | PointerMoveMsg (Float, Float)
    | PointerUpMsg
    | PointerOutMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchFunctions ->
            ( { model | functions = RemoteData.Loading }, fetchFunctions )

        FunctionsReceived response ->
            ( { model 
                | functions = response
                , positions = initPositions (RemoteData.withDefault [] response) 
                }
                , Cmd.none 
            )

        PointerDownMsg name pointer ->
            -- register pointerPosOnDown
            ( { 
                model 
                | pointerPos = pointer.clientPos
                , pointerPosOnDown = Just pointer.clientPos
                , relPointerPosOnDown = Just 
                    (relativePosTo pointer.clientPos (getPosOf name model.positions)
                    )
                , dragging = Just name 
                }
                , Cmd.none 
            )

        PointerMoveMsg pos -> 
            case model.dragging of
                Just name -> 
                    ( { model | pointerPos = pos, positions = updatePositions model pos }
                    , Cmd.none
                    )
                Nothing ->
                    ( { model | pointerPos = pos }, Cmd.none )
            
        PointerUpMsg ->
            ( { model 
                | dragging = Nothing
                , pointerPosOnDown = Nothing
                , relPointerPosOnDown = Nothing
                }
                , Cmd.none
            )

        PointerOutMsg ->
            ( model
                , Cmd.none
            )

relativePosTo : Position -> Position -> Position
relativePosTo posA posB =
    (Tuple.first posA - Tuple.first posB, Tuple.second posA - Tuple.second posB)

initPositions : List Function -> List FunctionPosition
initPositions functions =
    List.map 
        (\f -> (f.name, getPosOf f.name []) )
        functions

updatePositions : Model -> Position -> List FunctionPosition
updatePositions model pointerPos =
    if List.length model.positions <= 0 then -- init all positions
        initPositions (RemoteData.withDefault [] model.functions)
    else
        List.map 
            (\fp -> (Tuple.first fp, defineNewPosition (Tuple.first fp) model pointerPos)) 
            model.positions

defineNewPosition : String -> Model -> Position -> Position
defineNewPosition name model pointerPos =
    if isDragged name model.dragging then
        let
            relPos = Maybe.withDefault (0, 0) model.relPointerPosOnDown
        in
            -- pointer.clientPos - diffPosBetween boxPos pointer.clientPos
            ( Tuple.first pointerPos - Tuple.first relPos
            , Tuple.second pointerPos - Tuple.second relPos
            )
    else
        getPosOf name model.positions

getPosOf : String -> List FunctionPosition -> Position
getPosOf name positions =
    let
        funcPos = getTupleOf name positions
    in
        case funcPos of
            Just fp -> Tuple.second fp
            Nothing -> (100, 50) -- Default position

getTupleOf : String -> List FunctionPosition -> Maybe FunctionPosition
getTupleOf name positions =
    List.head (List.filter (\fp -> Tuple.first fp == name) positions)

isDragged : String -> Maybe String -> Bool
isDragged name dragging =
    name == Maybe.withDefault "" dragging

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pointerPosOnDown of
        Just pos -> BrEv.onMouseUp (Decode.succeed PointerUpMsg)
        Nothing -> Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Visual PHP"
    , body =
        [ div []
            [ text "New Application" ]
        , viewFunctionsOrError model
        ]
    }

viewFunctionsOrError : Model -> Html Msg
viewFunctionsOrError model =
    case model.functions of
        RemoteData.NotAsked ->
            text "Meh."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success functions ->
            viewFunctions functions model.positions

        RemoteData.Failure httpError ->
            text (buildErrorMessage httpError)

viewFunctions : List Function -> List FunctionPosition -> Html Msg
viewFunctions functions positions = 
    div [ Pointer.onUp (\_ -> PointerUpMsg) ] 
        [ svg [ width "100%", height "600", viewBox "0 0 800 600" ] 
            (List.map (\f -> viewFunction f positions) functions)
        ]
        

viewFunction : Function -> List FunctionPosition -> Svg Msg
viewFunction function positions = 
    let 
        funcPos = getPosOf function.name positions
        params =
            String.join ", " 
                ( List.map (\p -> p.type_) function.params
                )
        declaration = 
            function.name ++ " : (" ++ params ++ ") -> " ++ function.type_
    in
        Svg.foreignObject 
            [ width "350"
            , height "150"
            , x (String.fromFloat <| Tuple.first funcPos)
            , y (String.fromFloat <| Tuple.second funcPos)
            , id <| functionContainerId function.name
            ] 
            [ div 
                [ Pointer.onDown (\ev -> PointerDownMsg function.name ev.pointer)
                , Pointer.onUp (\_ -> PointerUpMsg)
                , Pointer.onMove (\ev -> PointerMoveMsg ev.pointer.clientPos)
                , Pointer.onOut (\_ -> PointerOutMsg)
                ] 
                [ p [] [ text declaration ]
                , p [ attribute "style" "white-space: pre" ] [ text function.source ]
                ]
            ]

functionContainerId : String -> String
functionContainerId id =
    "cont-" ++ id


type alias Function =
    { name : String
    , type_ : String
    , params : List Param
    , source : String
    }

type alias Param = 
    { name : String
    , type_ : String
    }

functionDecoder : Decoder Function
functionDecoder =
    Decode.succeed Function
        |> required"name" string
        |> required "type" string
        |> required "params" (list paramDecoder)
        |> required "source" string
paramDecoder : Decoder Param
paramDecoder =
    Decode.succeed Param
        |> required "name" string
        |> required "type" string


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message