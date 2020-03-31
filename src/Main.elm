module Main exposing(..)

import Http
import Browser
import Html exposing (Html, div, text, ul, li, input, button, s,  span, hr, h3, br)
import Html.Attributes exposing(type_, placeholder, value, class)
import Html.Events exposing(onInput, onClick)
import Json.Encode
import Json.Decode exposing (Decoder, field, string, list, bool, map3)

-- MAIN
main : Program () Model Msg
main =
    Browser.element { 
        init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
    }


-- partial type
type alias TodoList = List Todo


-- partial type
type alias Todo =
    { id :  String
    , name : String
    , isDone : Bool }


-- the state model
type alias Model = 
    { todos: TodoList
    , newTodo: Todo
    , requestState: String
    }


-- here we define the intial state for our application
init: () ->  (Model, Cmd Msg)
init _ = 
    ({ todos = [
        { id = "", name = "No Interwebs?", isDone  = False} ]
    , newTodo = { id = "", name = "", isDone = False}
    , requestState = "INIT"
    }, getTodos)
    

-- here we define all the type of actions we have
type Msg
    = NoOp 
    | Add | SetTodoName String | SetTodoIsDone Todo | Clear 
    | GetTodos | GotTodos (Result Http.Error TodoList) | GotAddTodo (Result Http.Error  ())
    | AddTodo | RemoveTodo String | GotRemoveTodo (Result Http.Error ())
    | GotUpdateTodo (Result Http.Error ()) | UpdateTodo Todo
    | GotClearTodos (Result Http.Error ())


-- the update cycle much like Redux. Takes a msg and a model then returns the model and optional command
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        
        SetTodoName todoName ->
            ({ model | newTodo = { id = "", name = todoName, isDone = False } }, Cmd.none)
        
        Add ->
            if model.newTodo.name == "" then
                (model, Cmd.none)
            else
                ({ model | todos = List.append model.todos [{ id = String.fromInt (List.length model.todos), name = model.newTodo.name, isDone = False }]
                , newTodo = { id = "", name = "", isDone = False }}, addTodo model.newTodo.name)
        
        SetTodoIsDone todo ->
            let
                setIsDone t =
                    if t.id == todo.id then
                        { todo | isDone = not todo.isDone}
                    else 
                        t
            in
                ({ model | todos = List.map setIsDone model.todos }, updateTodo todo)
        Clear ->
            ({ model | todos = List.filter (\t -> not t.isDone) model.todos }, clearTodos)

        GetTodos ->
            ({model | requestState = "PENDING"},
            getTodos)

        GotTodos result ->
            case result of
                Ok todos ->
                    ({ model | todos = todos, requestState = "SUCCESS"}, Cmd.none)
                Err _ ->
                    ({ model | requestState = "FAILED" }, Cmd.none)
        
        GotAddTodo _ ->
            (model, getTodos)
        
        AddTodo ->
            (model, addTodo model.newTodo.name)
        
        GotRemoveTodo _ ->
            (model, getTodos)
        
        RemoveTodo todoId -> 
            (model, removeTodo todoId)

        GotUpdateTodo _ ->
            (model, Cmd.none)
  
        UpdateTodo todo ->
            (model, Cmd.none)
        
        GotClearTodos _ ->
            (model, Cmd.none)


-- subscription setup for now i dont need any subscriptions 
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- just a partial render function
renderTodos: Model -> Html Msg
renderTodos model =
    if model.requestState == "SUCCESS" then
        ul [] (List.map renderTodo model.todos)
    else if model.requestState == "INIT" || model.requestState == "PENDING" then
        h3 [] [text "Loading..."]
    else
        h3 [] [text "No data received"]


-- just a partial render function
renderTodo: Todo -> Html Msg
renderTodo todo =
    if todo.isDone then
        li [class "todo closed"] [
            s [] [ text ("#" ++ todo.id ++ " | " ++ todo.name )]
            , div [] [ button [ class "re-open", onClick (SetTodoIsDone todo) ] [ text "Re-Open"] ]
        ]
    else
       li [class "todo open"] [
            span [] [ text ("#" ++ todo.id ++ " | " ++ todo.name )]
            , br [] []
            , div [] [ button [ class "complete", onClick (SetTodoIsDone todo) ] [ text "Complete"] ]
        ]


-- the view of our application it takes Model and returns Html 
view: Model -> (Html Msg)
view model =
    div [] [
        div [] [text "List"]
        , input [type_ "text", placeholder "Todo name", onInput SetTodoName, value model.newTodo.name ] []
        , hr [] []
        , div [class "todo-controls"] [
            button [ class "clear", onClick Clear ] [ text "Clear finished" ]
            , button [ class "add", onClick Add ] [ text "Add Todo"]
        ]
        , h3 [] [ text (if List.length model.todos == 0 then "No Todos" else "") ]
        , renderTodos model
    ]


-- command we can call that performs an HTTP Request and executes a callback
getTodos : Cmd Msg
getTodos =
    Http.get
    { url = "http://localhost:3000/todos"
    , expect = Http.expectJson GotTodos decodeTodos }


-- Elm cant read json so we need to decode the list
decodeTodos : Decoder TodoList
decodeTodos =
    list decodeTodo


-- Elm cant read json so we need to parse every field to the expected type
decodeTodo : Decoder Todo
decodeTodo = 
    map3 Todo
        (field "_id" string)
        (field "name" string)
        (field "isDone" bool)


-- we cant just post Elm data as json so we need to encode it to the right value
encodeTodo : String -> Json.Encode.Value
encodeTodo value =
    Json.Encode.object [ ("name", Json.Encode.string value) ] 

 
-- command we can call that will perform an HTTP Request and executes a callback
addTodo : String -> Cmd Msg
addTodo newTodo = 
    Http.post
    { url = "http://localhost:3000/todos"
    , body = Http.jsonBody (encodeTodo newTodo)
    , expect = Http.expectWhatever GotAddTodo }

removeTodo : String -> Cmd Msg
removeTodo todoId =
    Http.request
    { method = "DELETE"
    , url = "http://localhost:3000/todos/" ++ todoId
    , expect = Http.expectWhatever GotRemoveTodo 
    , body = Http.emptyBody
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    }

updateTodo : Todo -> Cmd Msg
updateTodo todo =
    Http.request
    { method = "PUT"
    , url = "http://localhost:3000/todos/" ++ todo.id ++ "?isDone=" ++ (if todo.isDone then "false" else "true")
    , body = Http.emptyBody
    , expect = Http.expectWhatever GotUpdateTodo
    , headers = []
    , timeout = Nothing
    , tracker = Nothing 
    }

clearTodos : Cmd Msg
clearTodos =
    Http.post
    { url = "http://localhost:3000/todos/remove"
    , expect = Http.expectWhatever GotClearTodos
    , body = Http.emptyBody }