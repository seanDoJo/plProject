module Planner exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

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
  , plannerItems : List (Html Msg)
  }

init : (Model, Cmd Msg)
init =
  (Model Start [], Cmd.none)

-- UPDATE

type Msg
  = StateUpdate State
  | Dummy

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StateUpdate newState -> (
      case model.state of
        Planner -> 
          if newState /= PlannerCreate then ({model | state = newState}, Cmd.none)
          else (model, Cmd.none)
        _ -> ({model | state = newState}, Cmd.none)
    )

    Dummy -> (model, Cmd.none)

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
--    ul [ class "sidebar-nav" ] ((li [ class "sidebar-brand" ] [ a [] [ text "Dynamic Planner" ] ]) :: (List.map (\n -> li [] [ n ]) (menuItems)))

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
      listItems = List.map (\n -> li [] [ n ]) (plannerItems)
    in
      ul [] listItems
