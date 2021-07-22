module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as A exposing (checked, class, classList, href, type_, value)
import Html.Events as E
import Json.Decode as Dec
import Task
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


type alias Model =
    { key : Nav.Key
    , inputText : String
    , filter : Filter
    , editingTodo : Maybe { id : Int, text : String }
    , todos : List TodoItem
    }


urlToFilter : Url.Url -> Filter
urlToFilter url =
    case url.fragment of
        Just "/completed" ->
            Completed

        Just "/active" ->
            Active

        _ ->
            All


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , inputText = ""
      , editingTodo = Nothing
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
    | DeletedTodo TodoItem
    | ToggledTodo TodoItem
    | ClickedEditTodo TodoItem
    | EditingTodoInput String
    | BlurredEditingInput
    | SavedEditingInput
    | InputFocused (Result Browser.Dom.Error ())


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
                    , todos = model.todos ++ [ { text = model.inputText, completed = False, id = maxId + 1 } ]
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

        ClickedEditTodo item ->
            ( { model | editingTodo = Just { id = item.id, text = item.text } }
            , Task.attempt InputFocused <| Browser.Dom.focus (getEditingInputId item)
            )

        EditingTodoInput value ->
            ( case model.editingTodo of
                Nothing ->
                    model

                Just editingTodo ->
                    { model | editingTodo = Just { editingTodo | text = value } }
            , Cmd.none
            )

        BlurredEditingInput ->
            ( { model | editingTodo = Nothing }
            , Cmd.none
            )

        SavedEditingInput ->
            ( case model.editingTodo of
                Nothing ->
                    model

                Just editingTodo ->
                    { model
                        | editingTodo = Nothing
                        , todos =
                            model.todos
                                |> List.map
                                    (\todo ->
                                        if todo.id /= editingTodo.id then
                                            todo

                                        else
                                            { todo | text = editingTodo.text }
                                    )
                    }
            , Cmd.none
            )

        InputFocused _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewHeader : String -> Html Msg
viewHeader inputText =
    header [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ A.autofocus True
            , class "new-todo"
            , A.placeholder "What needs to be done?"
            , value inputText
            , E.onInput InputText
            , onEnter SubmittedTodo
            ]
            []
        ]


getEditingInputId : TodoItem -> String
getEditingInputId { id } =
    "item-" ++ String.fromInt id


viewTodoItem : Maybe { id : Int, text : String } -> TodoItem -> Html Msg
viewTodoItem mEditingTodo item =
    let
        editing =
            case mEditingTodo of
                Just { id } ->
                    id == item.id

                Nothing ->
                    False

        editingInputValue =
            case mEditingTodo of
                Just { text } ->
                    text

                Nothing ->
                    ""
    in
    li [ classList [ ( "completed", item.completed ), ( "editing", editing ) ] ]
        [ div [ class "view" ]
            [ input
                [ checked item.completed
                , class "toggle"
                , type_ "checkbox"
                , E.onCheck (\_ -> ToggledTodo item)
                ]
                []
            , label [ E.onDoubleClick (ClickedEditTodo item) ] [ text item.text ]
            , button
                [ E.onClick (DeletedTodo item)
                , class "destroy"
                ]
                []
            ]
        , input
            [ class "edit"
            , value editingInputValue
            , E.onInput EditingTodoInput
            , E.onBlur BlurredEditingInput
            , onEnter SavedEditingInput
            , A.id (getEditingInputId item)
            ]
            []
        ]


viewMain : Model -> Html Msg
viewMain model =
    section [ class "main" ]
        [ input [ class "toggle-all", A.id "toggle-all", type_ "checkbox" ] []
        , label [ A.for "toggle-all" ] [ text "Mark all as complete" ]
        , ul [ class "todo-list" ] (model.todos |> List.map (viewTodoItem model.editingTodo))
        ]


viewFooter : Model -> Html msg
viewFooter model =
    let
        viewFooterItem filter href_ label =
            li []
                [ a
                    [ classList [ ( "selected", filter == model.filter ) ]
                    , href href_
                    ]
                    [ text label ]
                ]
    in
    footer [ class "footer" ]
        [ span [ class "todo-count" ]
            [ strong [] [ text "0" ]
            , text "item left"
            ]
        , ul [ class "filters" ]
            [ viewFooterItem All "#/" "All"
            , viewFooterItem Active "#/active" "Active"
            , viewFooterItem Completed "#/completed" "Completed"
            ]
        , button [ class "clear-completed" ] [ text "Clear completed" ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ section [ class "todoapp" ]
            [ viewHeader model.inputText
            , case model.todos of
                [] ->
                    text ""

                _ ->
                    viewMain model
            , case model.todos of
                [] ->
                    text ""

                _ ->
                    viewFooter model
            ]
        ]
    }


onEnter : msg -> Attribute msg
onEnter msg =
    let
        enterKeyCode =
            13

        decoder =
            Dec.field "keyCode" Dec.int
                |> Dec.andThen
                    (\keyCode ->
                        if keyCode == enterKeyCode then
                            Dec.succeed
                                { message = msg
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        else
                            Dec.fail ""
                    )
    in
    E.custom "keydown" decoder


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
