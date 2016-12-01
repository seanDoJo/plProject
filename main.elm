module Planner exposing (..)

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import List

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type State
  = Start
  | PlannerCreate
  | Planner
  | OfficeHours
  | Links

type alias Model =
  { state : State
  , plannerItems : List ((Html Msg, Maybe Date))
  , newTaskName : String
  , date : Maybe Date
  , datePicker : DatePicker.DatePicker
  }

init : (Model, Cmd Msg)
init =
  let
    isDisabled date = flip List.member [ Sat, Sun ] (dayOfWeek date)
    ( datePicker, datePickerFx ) =
      DatePicker.init
        { defaultSettings
            | isDisabled = isDisabled
            , inputClassList = [ ( "form-control", True ) ]
            , inputName = Just "date"
    }
  in
    (Model Start [] "" Nothing datePicker, Cmd.map ToDatePicker datePickerFx)

refreshDatePicker : (DatePicker.DatePicker, Cmd Msg)
refreshDatePicker =
  let
    isDisabled date = flip List.member [ Sat, Sun ] (dayOfWeek date)
    ( datePicker, datePickerFx ) =
      DatePicker.init
        { defaultSettings
            | isDisabled = isDisabled
            , inputClassList = [ ( "form-control", True ) ]
            , inputName = Just "date"
    }
  in
    (datePicker, Cmd.map ToDatePicker datePickerFx)

-- UPDATE

type Msg
  = StateUpdate State
  | NewTaskName String
  | NewTask
  | ToDatePicker DatePicker.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StateUpdate newState -> ({model | state = newState}, Cmd.none)

    NewTaskName name -> ({model | newTaskName = name}, Cmd.none)

    NewTask -> 
      let
        (dp, c) = refreshDatePicker
        uitems = updatePlanner model
      in
        ({model | newTaskName = "", plannerItems = uitems, state = Planner, date = Nothing, datePicker = dp}, c)

    ToDatePicker msg ->
      let
        ( newDatePicker, datePickerFx, mDate ) =
          DatePicker.update msg model.datePicker
        date =
          case mDate of
            Nothing ->
              model.date
            date ->
              date
      in
        ({model | date = date, datePicker = newDatePicker},
          Cmd.map ToDatePicker datePickerFx)

-- VIEW

view : Model -> Html Msg
view model =
  div [ id "wrapper" ]
  [ div [ id "sidebar-wrapper" ] [ generateSidebar model ]
  , div [ id "page-content-wrapper" ] [ div [ class "container-fluid" ] [ generateContent model ] ]
  ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

generateSidebar : Model -> Html Msg
generateSidebar model =
  let 
    plannerItem 
      = (
        if model.state == Planner then li [ class "selected-nav" ] [ a [ onClick (StateUpdate Planner)  ] [ text "Planner" ] ]
        else li [] [ a [ onClick (StateUpdate Planner)  ] [ text "Planner" ] ]
      )
  in
  let
    officeItem 
      = (
        if model.state == OfficeHours then li [ class "selected-nav" ] [ a [ onClick (StateUpdate OfficeHours)  ] [ text "Office Hours" ] ]
        else li [] [ a [ onClick (StateUpdate OfficeHours)  ] [ text "Office Hours" ] ]
      )
  in
  let
    linksItem 
      = (
        if model.state == Links then li [ class "selected-nav" ] [ a [ onClick (StateUpdate Links)  ] [ text "Links" ] ]
        else li [] [ a [ onClick (StateUpdate Links)  ] [ text "Links" ] ]
      )
  in
  let menuItems =
    [ plannerItem
    , officeItem
    , linksItem
    ]
  in
    ul [ class "sidebar-nav" ] ((li [ class "sidebar-brand" ] [ a [] [ text "Dynamic Planner" ] ]) :: menuItems)

generateContent : Model -> Html Msg
generateContent model =
  case model.state of
    Start ->
       div []
        [ div [ class "jumbotron" ] [ h1 [] [ text "Your Planner Made Easy" ] ]
        , button [ class "btn btn-primary btn-lg", onClick (StateUpdate Planner) ] [ text "Get Started Now!" ]
        ]
    Planner ->
       div []
        [ div [ class "jumbotron" ] [ h1 [] [ text "Items on Your Agenda" ] ]
        , generatePlannerItems model
        ]
    PlannerCreate ->
        div []
          [ h2 [ class "display-1" ] [ text "Add a new item to your agenda" ]
          , h3 [ class "text-muted" ] [ text "fill out the form below to create a new task" ]
          , div [ style [ ("margin-top", "10%") ] ] [ generateNewPlannerItem model ]
          ]
    _ ->
       div []
        [ h1 [] [ text "Not Implemented Yet!" ]
        ]

generatePlannerItems : Model -> Html Msg
generatePlannerItems {plannerItems} =
  if (List.length plannerItems) == 0 then
    div [ style [ ("width", "100%") ] ] 
    [ div [ class "container noplanning" ] [ h3 [ style [ ("color", "white") ] ] [ text "you don't currently have anything today (hooray!)" ] ]
    , button [ class "btn btn-info btn-lg buttn-rgt", onClick (StateUpdate PlannerCreate) ] [ text "Click Here to Make More!" ]
    ]
  else
    let
      listItems = List.map (\(n,d) -> li [] [ n ]) (List.sortWith comparePlannerItems plannerItems)
    in
      div [ style [ ("width", "100%") ] ]
      [ div [ class "container noplanning" ] [ ul [] listItems ]
      , button [ class "btn btn-info btn-lg buttn-rgt", onClick (StateUpdate PlannerCreate) ] [ text "Click Here to Make More!" ]
      ]

generateNewPlannerItem : Model -> Html Msg
generateNewPlannerItem model =
  div []
  [ div [ class "form-group" ] 
    [ label [ style [ ("for", "newName") ] ] [ text "Task Name" ]
    , input [ type_ "text", placeholder "Task Name", onInput NewTaskName, class "form-control", id "newName" ] [] 
    ]
  , div [ class "form-group" ]
    [ label [] [ text "Due Date" ]
    , Html.map ToDatePicker (DatePicker.view model.datePicker)
    ]
  , button [ class "btn btn-primary btn-lg", onClick NewTask ] [ text "Create" ]
  ]

updatePlanner : Model -> List ((Html Msg, Maybe Date))
updatePlanner model =
  let
    newItem = text model.newTaskName
  in
    (newItem, model.date) :: model.plannerItems

comparePlannerItems a b =
  let
    (a1, dt1) = a
    (a2, dt2) = b
  in
    case (dt1, dt2) of
      (Just d1, Just d2) ->
        compare (Date.toTime d1) (Date.toTime d2)
      _ -> EQ
