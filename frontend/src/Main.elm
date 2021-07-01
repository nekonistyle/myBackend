module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Generated.Api exposing (..)


-- Api.Memo

type MemoInput
    = TitleInput String
    | ContentInput String

updateMemo : Memo -> MemoInput -> Memo
updateMemo memo input =
    case input of
        TitleInput title ->
            { memo | memoTitle = title }

        ContentInput content ->
            { memo | memoContent = content }

inputMemoFormat : (MemoInput -> msg) -> Memo -> Html msg
inputMemoFormat toMsg memo =
    div []
        [ input [ placeholder "title"
                , value memo.memoTitle
                , onInput (toMsg << TitleInput)
                ]
              []
        , input [ placeholder "content"
                , value memo.memoContent
                , onInput (toMsg << ContentInput)
                ]
              []
        ]

viewMemo : Memo -> Html Msg
viewMemo memo =
    div []
        [ text ( memo.memoTitle ++ " / " ++ memo.memoContent )
        ]


--MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type Model
    = Loading
    | Failure Http.Error
    | CreateMemo Memo
    | ViewPostedMemo Memo
    | ViewAllMemos (List Memo)


-- INIT

init : () -> (Model, Cmd Msg)
init _ =
    (initModel, getAllMemos GotAllMemos)

initModel : Model
initModel = Loading

initMemo : Memo
initMemo = {memoTitle = "", memoContent = ""}

initMemoChange : MemoInput
initMemoChange = TitleInput ""

-- UPDATE

type Msg
    = ChangeMemo Memo MemoInput
    | PostMemo Memo
    | GotPostedMemo (Result Http.Error Memo)
    | GetAllMemos
    | GotAllMemos (Result Http.Error (List Memo))



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeMemo memo change ->
            (CreateMemo (updateMemo memo change), Cmd.none)

        PostMemo memo ->
            (Loading, postPostMemo memo GotPostedMemo)

        GotPostedMemo result ->
            case result of
                Ok memo ->
                    (ViewPostedMemo memo, Cmd.none)

                Err err ->
                    (Failure err, Cmd.none)

        GetAllMemos ->
            (Loading, getAllMemos GotAllMemos)

        GotAllMemos result ->
            case result of
                Ok memos ->
                    (ViewAllMemos memos, Cmd.none)

                Err err ->
                    (Failure err, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


--VIEW

view : Model -> Html Msg
view model =
    div []
        [ viewData model ]

createMemoButton : Html Msg
createMemoButton =
    button [ onClick (ChangeMemo initMemo initMemoChange) ]
        [ text "Create New Memo" ]

viewAllMemos : Html Msg
viewAllMemos =
    div []
        [ button [ onClick GetAllMemos ] [ text "View All Memos" ]
        ]

viewData : Model -> Html Msg
viewData model =
    case model of
        Loading ->
            text "Loading..."

        Failure err ->
            div []
                [ text "Failure: "
                , text (httpErrorToString err)
                , viewAllMemos
                , createMemoButton
                ]

        CreateMemo memo ->
            div []
                [ h3 [] [ text "Create Memo" ]
                , createMemo memo
                , viewAllMemos
                ]

        ViewPostedMemo memo ->
            div []
                [ h3 [] [ text "Posted Memo" ]
                , h5 [] [ text "Title / Content" ]
                , viewMemo memo
                , viewAllMemos
                , createMemoButton
                ]

        ViewAllMemos memos ->
            div []
                [ h3 [] [ text "All Memos" ]
                , h5 [] [ text "Title / Content" ]
                , div [] (List.map viewMemo memos)
                , createMemoButton
                ]


createMemo : Memo -> Html Msg
createMemo memo =
    div []
        [ inputMemoFormat (ChangeMemo memo) memo
        , button [ onClick (PostMemo memo) ] [ text "Post" ]
        ]

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl cmt -> "BadUrl: " ++ cmt

        Http.Timeout -> "Timeout"

        Http.NetworkError -> "NetworkError"

        Http.BadStatus num -> "BadStatus: " ++ String.fromInt num

        Http.BadBody cmt -> "BadBody: " ++ cmt


