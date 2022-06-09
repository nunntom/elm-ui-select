module Internal.Effect exposing (Effect(..), batch, none, perform)

import Browser.Dom as Dom
import Internal.Placement exposing (Placement(..))
import Select.Msg exposing (Msg(..))
import Task exposing (Task)


type Effect
    = GetMenuHeightAndPlacement String
    | GetElementsAndScrollMenu String Int
    | Batch (List Effect)
    | None


none : Effect
none =
    None


batch : List Effect -> Effect
batch effects =
    Batch effects


perform : Effect -> Cmd (Msg a)
perform effect =
    case effect of
        GetMenuHeightAndPlacement id ->
            getMenuHeightAndPlacement id

        GetElementsAndScrollMenu id optionIdx ->
            getElementsAndScrollMenu id optionIdx

        Batch effects ->
            List.foldl (\eff cmds -> perform eff :: cmds) [] effects
                |> Cmd.batch

        None ->
            Cmd.none


getMenuHeightAndPlacement : String -> Cmd (Msg a)
getMenuHeightAndPlacement id =
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
        |> Task.attempt GotMenuHeightAndPlacement


getElementsAndScrollMenu : String -> Int -> Cmd (Msg a)
getElementsAndScrollMenu id highlightedOption =
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
        |> Task.attempt GotScrollMenuResult


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
