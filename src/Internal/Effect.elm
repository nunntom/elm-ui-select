module Internal.Effect exposing (Effect(..), batch, emitJust, map, mapEffect, none, perform, simulate)

import Browser.Dom as Dom
import Process
import Task exposing (Task)


type Effect effect msg
    = ScrollMenuToTop msg String
    | GetContainerAndMenuElements (Result Dom.Error { menu : Dom.Viewport, container : Dom.Element } -> msg) { containerId : String, menuId : String }
    | GetElementsAndScrollMenu msg { menuId : String, optionId : String }
    | Batch (List (Effect effect msg))
    | Request effect
    | Emit msg
    | Debounce (String -> msg) Float String
    | FocusInput String msg
    | None


none : Effect effect msg
none =
    None


batch : List (Effect effect msg) -> Effect effect msg
batch effects =
    Batch effects


perform : (effect -> Cmd msg) -> Effect effect msg -> Cmd msg
perform toCmd effect =
    case effect of
        ScrollMenuToTop msg id ->
            Dom.setViewportOf id 0 0
                |> Task.attempt (\_ -> msg)

        GetContainerAndMenuElements msg ids ->
            getContainerAndMenuElements msg ids

        GetElementsAndScrollMenu msg ids ->
            getElementsAndScrollMenu msg ids

        Batch effects ->
            List.foldl (\eff cmds -> perform toCmd eff :: cmds) [] effects
                |> Cmd.batch

        Request eff ->
            toCmd eff

        Emit msg ->
            Task.succeed ()
                |> Task.perform (\_ -> msg)

        Debounce msg delay val ->
            Process.sleep delay
                |> Task.perform (\_ -> msg val)

        FocusInput id msg ->
            Dom.focus id
                |> Task.attempt (\_ -> msg)

        None ->
            Cmd.none


simulate :
    { perform : (() -> msg) -> simulatedTask -> simulatedEffect
    , batch : List simulatedEffect -> simulatedEffect
    , sleep : Float -> simulatedTask
    }
    -> (effect -> simulatedEffect)
    -> Effect effect msg
    -> simulatedEffect
simulate conf simulateRequest effect =
    case effect of
        ScrollMenuToTop msg _ ->
            conf.sleep 0
                |> conf.perform (\_ -> msg)

        GetContainerAndMenuElements msg _ ->
            conf.sleep 0
                |> conf.perform
                    (\_ ->
                        msg
                            (Ok
                                { container = { scene = { width = 1163, height = 975 }, viewport = { x = 0, y = 0, width = 1163, height = 975 }, element = { x = 436, y = 452, width = 291, height = 71 } }
                                , menu =
                                    { scene = { width = 530, height = 9970 }
                                    , viewport = { x = 0, y = 0, width = 274, height = 0 }
                                    }
                                }
                            )
                    )

        GetElementsAndScrollMenu msg _ ->
            conf.sleep 0
                |> conf.perform (\_ -> msg)

        Batch effects ->
            List.foldl (\eff cmds -> simulate conf simulateRequest eff :: cmds) [] effects
                |> conf.batch

        Request eff ->
            simulateRequest eff

        Emit msg ->
            conf.sleep 0
                |> conf.perform (\_ -> msg)

        Debounce msg delay val ->
            conf.sleep delay
                |> conf.perform (\_ -> msg val)

        FocusInput _ msg ->
            conf.sleep 0
                |> conf.perform (\_ -> msg)

        None ->
            conf.batch []


getContainerAndMenuElements : (Result Dom.Error { menu : Dom.Viewport, container : Dom.Element } -> msg) -> { containerId : String, menuId : String } -> Cmd msg
getContainerAndMenuElements msg { containerId, menuId } =
    Task.map2
        (\container menu ->
            { container = container
            , menu = menu
            }
        )
        (Dom.getElement containerId)
        (Dom.getViewportOf menuId)
        |> Task.attempt msg


getElementsAndScrollMenu : msg -> { menuId : String, optionId : String } -> Cmd msg
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
        |> Task.attempt (\_ -> msg)


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
    if optionBottom > (menuHeight + menuViewPortY - 10) then
        optionBottom - menuHeight + 10

    else if optionTop < menuViewPortY + 10 then
        optionTop - 10

    else
        menuViewPortY


map : (msg -> msg2) -> Effect effect msg -> Effect effect msg2
map toMsg effect =
    case effect of
        ScrollMenuToTop msg id ->
            ScrollMenuToTop (toMsg msg) id

        GetContainerAndMenuElements msg ids ->
            GetContainerAndMenuElements (msg >> toMsg) ids

        GetElementsAndScrollMenu msg ids ->
            GetElementsAndScrollMenu (toMsg msg) ids

        Batch effects ->
            List.map (map toMsg) effects
                |> Batch

        Request eff ->
            Request eff

        Emit msg ->
            Emit (toMsg msg)

        Debounce msg delay val ->
            Debounce (msg >> toMsg) delay val

        FocusInput id msg ->
            FocusInput id (toMsg msg)

        None ->
            None


mapEffect : (effect -> effect2) -> Effect effect msg -> Effect effect2 msg
mapEffect toEffect effect =
    case effect of
        ScrollMenuToTop msg id ->
            ScrollMenuToTop msg id

        GetContainerAndMenuElements msg ids ->
            GetContainerAndMenuElements msg ids

        GetElementsAndScrollMenu msg ids ->
            GetElementsAndScrollMenu msg ids

        Batch effects ->
            List.map (mapEffect toEffect) effects
                |> Batch

        Request eff ->
            Request (toEffect eff)

        Emit msg ->
            Emit msg

        Debounce msg delay val ->
            Debounce msg delay val

        FocusInput id msg ->
            FocusInput id msg

        None ->
            None


emitJust : Maybe msg -> Effect effect msg
emitJust msg =
    case msg of
        Just msg_ ->
            Emit msg_

        Nothing ->
            none
