module Internal.Effect exposing (Effect(..), batch, none, perform, simulate)

import Browser.Dom as Dom
import Internal.Msg exposing (Msg(..))
import Process
import Task exposing (Task)


type Effect effect
    = GetContainerAndMenuElements { containerId : String, menuId : String }
    | GetElementsAndScrollMenu { menuId : String, optionId : String }
    | Batch (List (Effect effect))
    | Request effect
    | Debounce Float String
    | None


none : Effect effect
none =
    None


batch : List (Effect effect) -> Effect effect
batch effects =
    Batch effects


perform : (Msg a -> msg) -> (effect -> Cmd msg) -> Effect effect -> Cmd msg
perform tagger requestCmd effect =
    case effect of
        GetContainerAndMenuElements ids ->
            getContainerAndMenuElements (GotContainerAndMenuElements >> tagger) ids

        GetElementsAndScrollMenu ids ->
            getElementsAndScrollMenu (tagger NoOp) ids

        Batch effects ->
            List.foldl (\eff cmds -> perform tagger requestCmd eff :: cmds) [] effects
                |> Cmd.batch

        Request eff ->
            requestCmd eff

        Debounce delay val ->
            Process.sleep delay
                |> Task.perform (\_ -> tagger (InputDebounceReturned val))

        None ->
            Cmd.none


simulate :
    (Msg a -> msg)
    ->
        { perform : (() -> msg) -> simulatedTask -> simulatedEffect
        , batch : List simulatedEffect -> simulatedEffect
        , sleep : Float -> simulatedTask
        }
    -> (effect -> simulatedEffect)
    -> Effect effect
    -> simulatedEffect
simulate tagger conf simulateRequest effect =
    case effect of
        GetContainerAndMenuElements _ ->
            conf.sleep 0
                |> conf.perform
                    (\_ ->
                        GotContainerAndMenuElements
                            (Ok
                                { container = { scene = { width = 1163, height = 975 }, viewport = { x = 0, y = 0, width = 1163, height = 975 }, element = { x = 436, y = 452, width = 291, height = 71 } }
                                , menu =
                                    { scene = { width = 530, height = 9970 }
                                    , viewport = { x = 0, y = 0, width = 274, height = 0 }
                                    }
                                }
                            )
                            |> tagger
                    )

        GetElementsAndScrollMenu _ ->
            conf.sleep 0
                |> conf.perform (\_ -> tagger NoOp)

        Batch effects ->
            List.foldl (\eff cmds -> simulate tagger conf simulateRequest eff :: cmds) [] effects
                |> conf.batch

        Request eff ->
            simulateRequest eff

        Debounce delay val ->
            conf.sleep delay
                |> conf.perform (\_ -> tagger (InputDebounceReturned val))

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
    if optionBottom > (menuHeight + menuViewPortY) then
        optionBottom - menuHeight

    else if optionTop < menuViewPortY then
        optionTop

    else
        menuViewPortY
