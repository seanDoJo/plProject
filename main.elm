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
    StateUpdate newState -> ({model | state = newState}, Cmd.none)

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
  let menuItems =
    [ a [ onClick Dummy ] [ text "Planner" ]
    , a [ onClick Dummy ] [ text "Office Hours" ]
    , a [ onClick Dummy ] [ text "Links" ]
    ]
  in
    ul [ class "sidebar-nav" ] ((li [ class "sidebar-brand" ] [ a [] [ text "Dynamic Planner" ] ]) :: (List.map (\n -> li [] [ n ]) (menuItems)))

generateContent : Model -> Html Msg
generateContent model =
  case model.state of
    Start ->
       div []
        [ h1 [] [ text "Your Planner Made Easy" ]
        , button [ onClick (StateUpdate Planner) ] [ text "Get Started Now!" ]
        ]
    Planner ->
       div []
        [ h1 [] [ text "Items on Your Agenda" ]
        , generatePlannerItems model
        ]
    _ ->
       div []
        [ h1 [] [ text "Not Implemented Yet!" ]
        ]

generatePlannerItems : Model -> Html Msg
generatePlannerItems {plannerItems} =
  let
    listItems = List.map (\n -> li [] [ n ]) (plannerItems)
  in
    ul [] listItems
