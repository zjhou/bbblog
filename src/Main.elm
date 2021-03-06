module Main exposing (..)
import Browser
import Html exposing (Html, text, div)
import Html.Parser
import Html.Parser.Util
import Html.Attributes exposing (class)
import Http
import Json.Decode as D


type alias Post = { title:String, content:String }

post: D.Decoder Post
post =
  D.map2 Post
    (D.field "title" D.string)
    (D.field "content" D.string)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL
type Msg
  = GotPosts (Result Http.Error (List Post))

type Model
    = Failure
    | Loading
    | Success (List Post)

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "https://gblog.vercel.app/api/posts"
      , expect = Http.expectJson GotPosts (D.list post)
      }
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPosts result ->
      case result of
        Ok posts ->
          (Success posts, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []

renderBlog: (List Post) -> Html Msg
renderBlog pLst =
  div [ class "blog" ]
    [ div [][]
    , showPosts pLst
    , div [][]
    ]

showPost: Post -> Html Msg
showPost p =
  div [ class "post" ]
   [ div [ class "post-title" ][text p.title]
   , div [ class "post-content" ](textHtml p.content)
   ]

showPosts: (List Post) -> Html Msg
showPosts pLst =
  div [class "post-list"]
    (List.map showPost pLst)

spinner: Html Msg
spinner =
  div [ class "spinner" ]
    [ div [ class "square1"] []
    , div [ class "square2"] []
    , div [ class "square3"] []
    ]

view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "加载失败!"

    Loading ->
      spinner

    Success posts ->
      renderBlog posts
