module Internal.Effect exposing (Effect(..), batch, none, perform)

import Browser.Dom as Dom exposing (Element)
import Process
import Task exposing (Task)


type Effect effect msg
    = GetContainerAndMenuElements (Result Dom.Error { menu : Element, container : Element } -> msg) { containerId : String, menuId : String }
    | GetElementsAndScrollMenu (Result Dom.Error () -> msg) { menuId : String, optionId : String }
    | Batch (List (Effect effect msg))
    | Request effect
    | Debounce Float msg
    | None


none : Effect effect msg
none =
    None


batch : List (Effect effect msg) -> Effect effect msg
batch effects =
    Batch effects


perform : (effect -> Cmd msg) -> Effect effect msg -> Cmd msg
perform requestCmd effect =
    case effect of
        GetContainerAndMenuElements msg ids ->
            getContainerAndMenuElements msg ids

        GetElementsAndScrollMenu msg ids ->
            getElementsAndScrollMenu msg ids

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


getContainerAndMenuElements : (Result Dom.Error { menu : Element, container : Element } -> msg) -> { containerId : String, menuId : String } -> Cmd msg
getContainerAndMenuElements msg { containerId, menuId } =
    Task.map2
        (\container menu ->
            { container = container
            , menu = menu
            }
        )
        (Dom.getElement containerId)
        (Dom.getElement menuId)
        |> Task.attempt msg


getElementsAndScrollMenu : (Result Dom.Error () -> msg) -> { menuId : String, optionId : String } -> Cmd msg
getElementsAndScrollMenu msg { menuId, optionId } =
    Task.map3
        (\option menu menuViewport ->
            { option = option
            , menu = menu
            , menuViewport = menuViewport
            }
        )
        (Dom.getElement optionId)
        (Dom.getElement menuId)
        (Dom.getViewportOf menuId)
        |> Task.andThen (scrollMenuTask menuId)
        |> Task.attempt msg


scrollMenuTask : String -> { option : Dom.Element, menu : Dom.Element, menuViewport : Dom.Viewport } -> Task Dom.Error ()
scrollMenuTask id { option, menu, menuViewport } =
    calculateScrollTop
        { optionTop = option.element.y - menu.element.y + menuViewport.viewport.y
        , optionBottom =
            (option.element.y - menu.element.y + menuViewport.viewport.y)
                + option.element.height
        , menuViewPortY = menuViewport.viewport.y
        , menuHeight = menu.element.height
        }
        |> Dom.setViewportOf id 0


calculateScrollTop : { optionTop : Float, optionBottom : Float, menuViewPortY : Float, menuHeight : Float } -> Float
calculateScrollTop { optionTop, optionBottom, menuViewPortY, menuHeight } =
    if optionBottom > (menuHeight + menuViewPortY) then
        optionBottom - menuHeight

    else if optionTop < menuViewPortY then
        optionTop

    else
        menuViewPortY



{- map : (msg1 -> msg2) -> Effect effect msg1 -> Effect effect msg2
   map toMsg effect =
       case effect of
           GetContainerAndMenuElements msg id ->
               GetContainerAndMenuElements (msg >> toMsg) id

           GetElementsAndScrollMenu msg ids ->
               GetElementsAndScrollMenu (msg >> toMsg) ids

           Batch effects ->
               List.map (map toMsg) effects
                   |> Batch

           Request eff ->
               Request eff

           Debounce delay msg ->
               Debounce delay (toMsg msg)

           None ->
               None
-}
