module Generated.Api exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Memo  =
   { memoTitle: String
   , memoContent: String
   }

jsonDecMemo : Json.Decode.Decoder ( Memo )
jsonDecMemo =
   Json.Decode.succeed (\pmemoTitle pmemoContent -> {memoTitle = pmemoTitle, memoContent = pmemoContent})
   |> required "memoTitle" (Json.Decode.string)
   |> required "memoContent" (Json.Decode.string)

jsonEncMemo : Memo -> Value
jsonEncMemo  val =
   Json.Encode.object
   [ ("memoTitle", Json.Encode.string val.memoTitle)
   , ("memoContent", Json.Encode.string val.memoContent)
   ]


getAllMemos : (Result Http.Error  ((List Memo))  -> msg) -> Cmd msg
getAllMemos toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "allMemos"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecMemo))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postPostMemo : Memo -> (Result Http.Error  (Memo)  -> msg) -> Cmd msg
postPostMemo body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "postMemo"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncMemo body)
            , expect =
                Http.expectJson toMsg jsonDecMemo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
