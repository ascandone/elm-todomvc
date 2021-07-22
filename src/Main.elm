module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as A exposing (checked, class, href, type_, value)
import Html.Events as E
import Url


type alias TodoItem =
    { id : Int
    , text : String
    , completed : Bool
    }


type Filter
    = All
    | Completed
    | Active


type alias Flags =
    { todos : Maybe String
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , inputText : String
    , filter : Filter
    , todos : List TodoItem
    }


urlToFilter : Url.Url -> Filter
urlToFilter url =
    case url.fragment of
        Just "completed" ->
            Completed

        Just "active" ->
            Active

        _ ->
            All


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , inputText = ""
      , todos =
            case flags of
                _ ->
                    []
      , filter = urlToFilter url
      }
    , Cmd.none
    )


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | InputText String
    | SubmittedTodo
    | DeletedTodo { id : Int }
    | ToggledTodo { id : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | filter = urlToFilter url }
            , Cmd.none
            )

        InputText inputText ->
            ( { model | inputText = inputText }
            , Cmd.none
            )

        SubmittedTodo ->
            ( if String.isEmpty (String.trim model.inputText) then
                model

              else
                let
                    maxId =
                        model.todos
                            |> List.map .id
                            |> List.foldl max -1
                in
                { model
                    | inputText = ""
                    , todos = { text = model.inputText, completed = False, id = maxId + 1 } :: model.todos
                }
            , Cmd.none
            )

        DeletedTodo { id } ->
            ( { model
                | todos = model.todos |> List.filter (\todo -> todo.id /= id)
              }
            , Cmd.none
            )

        ToggledTodo { id } ->
            ( { model
                | todos =
                    model.todos
                        |> List.map
                            (\todo ->
                                if todo.id /= id then
                                    todo

                                else
                                    { todo | completed = not todo.completed }
                            )
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewHeader : Html msg
viewHeader =
    header [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input [ A.autofocus True, class "new-todo", A.placeholder "What needs to be done?" ] []
        ]


viewMain : Html msg
viewMain =
    section [ class "main" ]
        [ input [ class "toggle-all", A.id "toggle-all", type_ "checkbox" ] []
        , label [ A.for "toggle-all" ] [ text "Mark all as complete" ]
        , ul [ class "todo-list" ]
            [ text "\t\t\t\t\t"
            , li [ class "completed" ]
                [ div [ class "view" ]
                    [ input [ checked True, class "toggle", type_ "checkbox" ] []
                    , label [] [ text "Taste JavaScript" ]
                    , button [ class "destroy" ] []
                    ]
                , input [ class "edit", value "Create a TodoMVC template" ] []
                ]
            , li []
                [ div [ class "view" ]
                    [ input [ class "toggle", type_ "checkbox" ] []
                    , label []
                        [ text "Buy a unicorn" ]
                    , button [ class "destroy" ] []
                    ]
                , input [ class "edit", value "Rule the web" ]
                    []
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer [ class "footer" ]
        [ span [ class "todo-count" ]
            [ strong [] [ text "0" ]
            , text "item left"
            ]
        , ul [ class "filters" ]
            [ li [] [ a [ class "selected", href "#/" ] [ text "All" ] ]
            , li [] [ a [ href "#/active" ] [ text "Active" ] ]
            , li [] [ a [ href "#/completed" ] [ text "Completed" ] ]
            ]
        , button [ class "clear-completed" ] [ text "Clear completed" ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ section [ class "todoapp" ]
            [ viewHeader
            , viewMain
            , viewFooter
            ]
        ]
    }
