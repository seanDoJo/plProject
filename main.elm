module Planner exposing (..)

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, second)

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
  , completedItems : List ((Html Msg, Maybe Date))
  , newTaskName : String
  , date : Maybe Date
  , datePicker : DatePicker.DatePicker
  , currentTime : Time
  }

init : (Model, Cmd Msg)
init =
  let
    ( datePicker, datePickerFx ) =
      DatePicker.init
        { defaultSettings
            | inputClassList = [ ( "form-control", True ) ]
            , inputName = Just "date"
    }
  in
    (Model Start [] [] "" Nothing datePicker 0, Cmd.map ToDatePicker datePickerFx)

refreshDatePicker : (DatePicker.DatePicker, Cmd Msg)
refreshDatePicker =
  let
    ( datePicker, datePickerFx ) =
      DatePicker.init
        { defaultSettings
            | inputClassList = [ ( "form-control", True ) ]
            , inputName = Just "date"
    }
  in
    (datePicker, Cmd.map ToDatePicker datePickerFx)

-- UPDATE

type Msg
  = StateUpdate State
  | NewTaskName String
  | NewTask
  | CancelTask
  | ToDatePicker DatePicker.Msg
  | Tick Time
  | CompleteItem Int
  | RemoveItem Int
  | Replace Int

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

    CancelTask ->
      let
        (dp, c) = refreshDatePicker
      in
        ({model | newTaskName = "", state = Planner, date = Nothing, datePicker = dp}, c)

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

    Tick newTime ->
      ({model | currentTime = newTime}, Cmd.none)

    CompleteItem index ->
      let
        item = selectItem index model.plannerItems
        remaining = removeItem index model.plannerItems
        completed = updateCompleted item model
      in
        ({model | plannerItems = remaining, completedItems = completed}, Cmd.none)

    RemoveItem index ->
      ({ model | plannerItems = (removeItem index model.plannerItems) } , Cmd.none)

    Replace index ->
      let
        item = selectItem index model.completedItems
        remaining = removeItem index model.completedItems
        updated = updatePlannerItem item model
      in
        ({model | plannerItems = updated, completedItems = remaining}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [ id "wrapper" ]
  [ div [ id "sidebar-wrapper" ] [ generateSidebar model ]
  --, div [ id "page-content-wrapper" ] [ div [ class "container maincontainer" ] [ generateContent model ] ]
  , div [] [ div [ class "container maincontainer" ] [ generateContent model ] ]
  ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

generateSidebar : Model -> Html Msg
generateSidebar model =
  let 
    plannerItem 
      = (
        if model.state == Planner || model.state == PlannerCreate then li [ class "selected-nav" ] [ a [ onClick (StateUpdate Planner)  ] [ text "Planner" ] ]
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
          [ div [ class "jumbotron" ] [ h1 [] [ text "Items on Your Agenda" ] ]
          , generatePlannerItems model
          ]
    _ ->
       div []
        [ h1 [] [ text "Not Implemented Yet!" ]
        ]

generatePlannerItems : Model -> Html Msg
generatePlannerItems ({plannerItems, currentTime, state, datePicker} as model) =
  case state of
  Planner ->
          if (List.length plannerItems) == 0 then
            div [ style [ ("width", "100%") ] ] 
            [ h3 [ class "display-2" ] [ text "Upcoming Items" ]
            , div [ class "container noplanning" ] [ h3 [ style [ ("color", "white") ] ] [ text "you don't currently have anything to do (hooray!)" ] ]
            , div [ class "container rightside" ]
              [ button [ class "btn btn-info btn-lg buttn-rgt", onClick (StateUpdate PlannerCreate) ] [ text "Click Here to Make More!" ]
              , div [ class "completed" ] [ ul [ class "list-group" ] (generateCompleted model) ]
              ]
            ]
          else
            let
              listItems = 
                List.indexedMap (\ index e ->
                  let
                    (n, d) = e
                  in
                    li [ class "list-group-item" , style [ ("margin", "10px"), ("border-radius", "5px") ] ] 
                    [ div [ style [ ("width", "50%"), ("display", "inline-block") ] ] 
                      [ n
                      , (getDaysUntil d currentTime)
                      , formatDueDate d 
                      ]
                    , div [ class "pull-right", style [ ("width", "45%"), ("float", "right"), ("display", "inline-block") ] ]
                      [ button [ class "checkcontainer", onClick (CompleteItem index) ] 
                        [ img [ class "checkmark", src "checkmark1.png"] [] ]
                      , button [ class "xcontainer", onClick (RemoveItem index) ]
                        [ img [ class "checkmark", src "redx1.png" ] [] ] 
                      ] 
                    ] 
                ) (plannerItems)
            in
              div [ style [ ("width", "100%") ] ]
              [ h3 [ class "display-2" ] [ text "Upcoming Items" ]
              , div [ class "container noplanning" ] [ ul [ class "list-group" ] listItems ]
              , div [ class "container rightside" ]
                [ button [ class "btn btn-info btn-lg buttn-rgt", onClick (StateUpdate PlannerCreate) ] [ text "Click Here to Make More!" ]
                , div [ class "completed" ] [ ul [ class "list-group" ] (generateCompleted model) ]
                ]
              ]

  PlannerCreate ->
          if (List.length plannerItems) == 0 then
            div [] 
            [ h3 [ class "display-2" ] [ text "Upcoming Items" ]
            , div [ class "container noplanning" ] [ h3 [ style [ ("color", "white") ] ] [ text "you don't currently have anything today (hooray!)" ] ]
            , div [ class "container sideplanning" ]
              [ div [ class "form-group" ] 
                [ label [ style [ ("for", "newName") ] ] [ text "Task Name" ]
                , input [ type_ "text", placeholder "Task Name", onInput NewTaskName, class "form-control", id "newName" ] [] 
                ]
              , div [ class "form-group" ]
                [ label [] [ text "Due Date" ]
                , Html.map ToDatePicker (DatePicker.view datePicker)
                ]
              , div[ style [ ("margin-top", "10px") ] ]
                [ button [ class "btn btn-danger btn-lg buttn-lft" , onClick CancelTask ] [ text "Cancel" ]
                , validateNewTask model
                ]
              ]
            ]
          else
            let
              listItems = 
                List.map (\(n,d) -> 
                  li [ class "list-group-item" , style [ ("margin", "10px"), ("border-radius", "5px") ]] 
                  [ div [] 
                    [ n
                    , (getDaysUntil d currentTime)
                    , formatDueDate d 
                    ]
                  ] 
                ) (plannerItems)
            in
              div []
              [ h3 [ class "display-2" ] [ text "Upcoming Items" ]
              , div [ class "container noplanning" ] [ ul [ class "list-group" ] listItems ]
              , div [ class "container sideplanning" ]
                [ div [ class "form-group" ] 
                  [ label [ style [ ("for", "newName") ] ] [ text "Task Name" ]
                  , input [ type_ "text", placeholder "Task Name", onInput NewTaskName, class "form-control", id "newName" ] [] 
                  ]
                , div [ class "form-group" ]
                  [ label [] [ text "Due Date" ]
                  , Html.map ToDatePicker (DatePicker.view datePicker)
                  ]
                , div[ style [ ("margin-top", "10px") ] ]
                  [ button [ class "btn btn-danger btn-lg buttn-lft" , onClick CancelTask ] [ text "Cancel" ]
                  , validateNewTask model
                  ]
                ]
              ]
  _ -> text "ERROR"

validateNewTask : Model -> Html Msg
validateNewTask model =
  let
    exist = not (String.isEmpty (model.newTaskName))
  in
    case model.date of

    Just d ->
      if exist then
        button [ class "btn btn-info btn-lg buttn-rgt" , onClick NewTask ] [ text "Add Task" ]
      else
        button [ class "btn btn-info btn-lg buttn-rgt" , onClick NewTask, disabled True ] [ text "Add Task" ]

    Nothing -> button [ class "btn btn-info btn-lg buttn-rgt" , onClick NewTask, disabled True ] [ text "Add Task" ]


updatePlanner : Model -> List ((Html Msg, Maybe Date))
updatePlanner model =
  let
    newItem = h3 [ style [] ] [ text model.newTaskName ]
  in
    let
      newL = (newItem, model.date) :: model.plannerItems
    in
      List.sortWith comparePlannerItems newL

updatePlannerItem : (Html Msg, Maybe Date) -> Model -> List ((Html Msg, Maybe Date))
updatePlannerItem item model =
  let
    newL = item :: model.plannerItems
  in
    List.sortWith comparePlannerItems newL

updateCompleted : (Html Msg, Maybe Date) -> Model -> List ((Html Msg, Maybe Date))
updateCompleted item model =
  item :: model.completedItems

comparePlannerItems a b =
  let
    (a1, dt1) = a
    (a2, dt2) = b
  in
    case (dt1, dt2) of
      (Just d1, Just d2) ->
        compare (Date.toTime d1) (Date.toTime d2)
      _ -> EQ

formatDueDate : Maybe Date -> Html msg
formatDueDate d =
  case d of
    Just date ->
      let
        day = dayToString (Date.dayOfWeek date)
        nday = toString (Date.day date)
        month = monthToString (Date.month date)
        year = Date.year date
      in
        div [ style [ ("bottom", "0"), ("left", "0") ] ] [ text (day ++ ", " ++ month ++ " " ++ nday) ]
    Nothing -> text "ERROR"

dayToString : Date.Day -> String
dayToString d =
  case d of
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"

monthToString : Date.Month -> String
monthToString m =
  case m of
    Date.Jan -> "January"
    Date.Feb -> "February"
    Date.Mar -> "March"
    Date.Apr -> "April"
    Date.May -> "May"
    Date.Jun -> "June"
    Date.Jul -> "July"
    Date.Aug -> "August"
    Date.Sep -> "September"
    Date.Oct -> "October"
    Date.Nov -> "November"
    Date.Dec -> "December"

getDaysUntil : Maybe Date -> Float -> Html msg
getDaysUntil d dtn =
  case d of
    Just date ->
      let
        dt = Date.toTime date
        df = dt - dtn
      in
        if df <= 0 then
          span [ style [ ("color", "red") ] ] [ text "Task is Overdue!" ]
        else
          let
            days = milliToDays df
            weeks = milliToWeeks df
          in
            if weeks == "" && days == "" then
              span [ style [ ("color", "red") ] ] [ text "Due Today!" ]
            else
              let
                daysn = milliToDaysn df
                weeksn = milliToWeeksn df
              in
                if (weeksn <= 0) then
                  if (daysn <= 3) then
                    span [ style [ ("color", "red") ] ] [ text ("Due in " ++ weeks ++ days) ]
                  else
                    if (daysn <= 5) then
                      span [ style [ ("color", "orange") ] ] [ text ("Due in " ++ weeks ++ days) ]
                    else
                      span [ style [ ("color", "green") ] ] [ text ("Due in " ++ weeks ++ days) ]
                else
                  span [ style [ ("color", "green") ] ] [ text ("Due in " ++ weeks ++ days) ]
    Nothing -> text "ERROR"

milliToDays : Float -> String
milliToDays m =
  let
    days = (ceiling (m / (1000 * 60 * 60 * 24))) % 7
  in
    if days > 0 then
       (toString days) ++ " Days"
    else
      ""

milliToDaysn : Float -> Int
milliToDaysn m =
  (ceiling (m / (1000 * 60 * 60 * 24))) % 7

milliToWeeks : Float -> String
milliToWeeks w =
  let
    weeks = floor (w / (1000 * 60 * 60 * 24 * 7))
  in
    if weeks > 0 then
      (toString weeks) ++ " Weeks "
    else
      ""
milliToWeeksn : Float -> Int
milliToWeeksn w =
  floor (w / (1000 * 60 * 60 * 24 * 7))

removeItem : Int -> List ((Html Msg, Maybe Date)) -> List ((Html Msg, Maybe Date))
removeItem i s =
  let
    firstTaken = List.take i s
    firstDropped = List.drop (i+1) s
  in
    (firstTaken ++ firstDropped)

selectItem : Int -> List ((Html Msg, Maybe Date)) -> (Html Msg, Maybe Date)
selectItem i s =
  let
    firstTaken = List.take (i+1) s
    item = List.drop i firstTaken
  in
    case item of
      it::rest -> it
      _ -> (text "ERROR", Nothing)

generateCompleted : Model -> List(Html Msg)
generateCompleted model =
  List.indexedMap (\ index e ->
    let
      (n, d) = e
    in
      li [ class "list-group-item" , style [ ("margin", "10px"), ("border-radius", "5px") ] ] 
        [ div [ style [ ("width", "50%"), ("display", "inline-block") ] ] 
          [ n
          , text "Completed"
          ]
        , div [ class "pull-right", style [ ("width", "45%"), ("float", "right"), ("display", "inline-block") ] ]
          [ button [ class "ucontainer", onClick (Replace index) ] 
            [ img [ class "checkmark", src "undo1.png"] [] ]
          ] 
        ] 
  ) (model.completedItems)
