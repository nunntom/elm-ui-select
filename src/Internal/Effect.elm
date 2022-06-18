module Internal.Effect exposing (Effect(..), batch, map, none, perform)

import Browser.Dom as Dom
import Internal.Msg exposing (Msg(..))
import Internal.Placement exposing (Placement(..))
import Process
import Task exposing (Task)


type Effect eff msg
    = GetMenuHeightAndPlacement (Result Dom.Error ( Maybe Int, Placement ) -> msg) String
    | GetElementsAndScrollMenu (Result Dom.Error () -> msg) String Int
    | Batch (List (Effect eff msg))
    | Request eff
    | Debounce Float msg
    | None


none : Effect eff msg
none =
    None


batch : List (Effect eff msg) -> Effect eff msg
batch effects =
    Batch effects


perform : (eff -> Cmd msg) -> Effect eff msg -> Cmd msg
perform requestCmd effect =
    case effect of
        GetMenuHeightAndPlacement msg id ->
            getMenuHeightAndPlacement msg id

        GetElementsAndScrollMenu msg id optionIdx ->
            getElementsAndScrollMenu msg id optionIdx

        Batch effects ->
            List.foldl (\eff cmds -> perform requestCmd eff :: cmds) [] effects
                |> Cmd.batch

        Request eff ->
            requestCmd eff

        Debounce delay msg ->
            Process.sleep delay
                |> Task.perform (\_ -> msg)

        None ->
            Cmd.none


getMenuHeightAndPlacement : (Result Dom.Error ( Maybe Int, Placement ) -> msg) -> String -> Cmd msg
getMenuHeightAndPlacement msg id =
    Task.map2
        (\input menu ->
            let
                { above, below } =
                    calculateSpace input
            in
            if below < Basics.round menu.scene.height && above > below then
                ( Just <| Basics.min (Basics.round menu.scene.height) (above - 10)
                , Above
                )

            else
                ( Just <| Basics.min (Basics.round menu.scene.height) (below - 10)
                , Below
                )
        )
        (Dom.getElement (id ++ "-input"))
        (Dom.getElement (id ++ "-menu"))
        |> Task.attempt msg


getElementsAndScrollMenu : (Result Dom.Error () -> msg) -> String -> Int -> Cmd msg
getElementsAndScrollMenu msg id highlightedOption =
    Task.map3
        (\option menu menuViewport ->
            { option = option
            , menu = menu
            , menuViewport = menuViewport
            }
        )
        (Dom.getElement (optionId highlightedOption id))
        (Dom.getElement (id ++ "-menu"))
        (Dom.getViewportOf (id ++ "-menu"))
        |> Task.andThen (scrollMenuTask id)
        |> Task.attempt msg


optionId : Int -> String -> String
optionId i id =
    id ++ "-" ++ String.fromInt i


calculateSpace : Dom.Element -> { above : Int, below : Int }
calculateSpace { viewport, element } =
    { above = Basics.round (element.y - viewport.y)
    , below =
        Basics.round
            ((viewport.y + viewport.height)
                - (element.y + element.height)
            )
    }


scrollMenuTask : String -> { option : Dom.Element, menu : Dom.Element, menuViewport : Dom.Viewport } -> Task Dom.Error ()
scrollMenuTask id { option, menu, menuViewport } =
    let
        optionTop =
            option.element.y - menu.element.y + menuViewport.viewport.y

        optionBottom =
            optionTop + option.element.height

        scrollTop =
            if optionBottom > (menu.element.height + menuViewport.viewport.y) then
                optionBottom - menu.element.height

            else if optionTop < menuViewport.viewport.y then
                optionTop

            else
                menuViewport.viewport.y
    in
    Dom.setViewportOf (id ++ "-menu") 0 scrollTop


map : (msg1 -> msg2) -> Effect eff msg1 -> Effect eff msg2
map toMsg effect =
    case effect of
        GetMenuHeightAndPlacement msg id ->
            GetMenuHeightAndPlacement (msg >> toMsg) id

        GetElementsAndScrollMenu msg id optionIdx ->
            GetElementsAndScrollMenu (msg >> toMsg) id optionIdx

        Batch effects ->
            List.map (map toMsg) effects
                |> Batch

        Request eff ->
            Request eff

        Debounce delay msg ->
            Debounce delay (toMsg msg)

        None ->
            None
