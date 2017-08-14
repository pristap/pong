module Pong exposing (..)

-- import Elmo8.Pico8 as Pico8
-- import Elmo8.Console as Console exposing (..)

import Html exposing (div, text, h1)
import Html.Attributes exposing (style)
import Keyboard.Extra
import Elmo8.Assets
import Elmo8.GL.Display
import Elmo8.Scene
import Time exposing (..)
import AnimationFrame


--  Types & Aliases


assetAtlas : String
assetAtlas =
    "assets_atlas"


type Msg
    = AssetMsg Elmo8.Assets.Msg
    | DisplayMsg Elmo8.GL.Display.Msg
    | KeyboardMsg Keyboard.Extra.Msg
    | Tick Float


type alias Input =
    { pause : Bool
    , leftPaddleMove : Int
    , rightPaddleMove : Int

    -- , delta : Time.Time
    }


type alias Object a =
    { a
        | x : Int
        , y : Int
        , vx : Int
        , vy : Int
        , id : Int
        , sprite : Int
        , layer : Int
        , textureKey : String
    }


type alias Ball =
    Object {}


type alias Player =
    Object { score : Int }


type State
    = Play
    | Pause


type PlayerType
    = Left
    | Right


type alias Game =
    { state : State
    , leftPlayer : Player
    , rightPlayer : Player
    , ball : Ball
    , input : Input
    , assets : Elmo8.Assets.Model
    , display : Elmo8.GL.Display.Model
    , scene : Elmo8.Scene.Model
    , keyboardModel : Keyboard.Extra.Model
    }



--  Initial Code


halfScreen : Int
halfScreen =
    64


initialBall : Ball
initialBall =
    { x = halfScreen, y = halfScreen, vx = 180, vy = 180, id = 0, textureKey = assetAtlas, sprite = 2, layer = 2 }


initialPlayer : Player
initialPlayer =
    { x = 0, y = halfScreen, vx = 0, vy = 0, score = 0, id = 0, textureKey = assetAtlas, sprite = 1, layer = 2 }


playerForType : PlayerType -> Player
playerForType playerType =
    let
        player =
            initialPlayer
    in
        case playerType of
            Left ->
                { player
                    | x = 4
                    , id = 1
                    , sprite = 0
                }

            Right ->
                { player
                    | x = 120
                    , id = 2
                    , sprite = 1
                }


addDash : Int -> Elmo8.Scene.Model -> Elmo8.Scene.Model
addDash index scene =
    let
        ( updatedScene, _ ) =
            { x = 64
            , y = index * 8
            , sprite = 3
            , id = 10 + index
            , textureKey = assetAtlas
            , layer = 1
            }
                |> Elmo8.Scene.addSprite scene
    in
        updatedScene


addBackgroundTile : Int -> Elmo8.Scene.Model -> Elmo8.Scene.Model
addBackgroundTile index scene =
    let
        ( updatedScene, _ ) =
            { x = 8 * (rem index 16)
            , y = 8 * (index // 16)
            , sprite = 4
            , id = 30 + index
            , textureKey = assetAtlas
            , layer = 0
            }
                |> Elmo8.Scene.addSprite scene
    in
        updatedScene


initialGame : ( Game, Cmd Msg )
initialGame =
    let
        ( assetModel, assetMsg ) =
            Elmo8.Assets.init

        ( updatedAssetModel, spriteMsg ) =
            Elmo8.Assets.loadTexture assetModel assetAtlas [ "Pong.png" ]

        ( displayModel, displayMsg ) =
            Elmo8.GL.Display.init

        ( keyboardModel, keyboardMsg ) =
            Keyboard.Extra.init

        ( scene, ball ) =
            Elmo8.Scene.addSprite Elmo8.Scene.init initialBall

        ( scene2, leftPlayer ) =
            Elmo8.Scene.addSprite scene (playerForType Left)

        ( scene3, rightPlayer ) =
            Elmo8.Scene.addSprite scene2 (playerForType Right)

        scene4 =
            List.foldr addDash scene3 (List.range 0 15)

        scene5 =
            List.foldr addBackgroundTile scene4 (List.range 0 255)

        input =
            { pause = False
            , leftPaddleMove = 0
            , rightPaddleMove = 0
            }
    in
        { state = Pause
        , ball = ball
        , leftPlayer = leftPlayer
        , rightPlayer = rightPlayer
        , input = input
        , assets = updatedAssetModel
        , display = displayModel
        , scene = scene5
        , keyboardModel = keyboardModel
        }
            ! [ Cmd.map AssetMsg assetMsg
              , Cmd.map AssetMsg spriteMsg
              , Cmd.map DisplayMsg displayMsg
              , Cmd.map KeyboardMsg keyboardMsg
              ]



--  Update


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        AssetMsg assetMsg ->
            let
                ( updatedAssets, newMsg ) =
                    Elmo8.Assets.update assetMsg game.assets
            in
                { game | assets = updatedAssets } ! [ Cmd.map AssetMsg newMsg ]

        DisplayMsg displayMsg ->
            let
                ( updatedDisplay, newMsg ) =
                    Elmo8.GL.Display.update displayMsg game.display
            in
                { game | display = updatedDisplay } ! [ Cmd.map DisplayMsg newMsg ]

        KeyboardMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.Extra.update keyMsg game.keyboardModel

                arrows =
                    Keyboard.Extra.arrows keyboardModel

                wasd =
                    Keyboard.Extra.wasd keyboardModel

                space =
                    Keyboard.Extra.isPressed Keyboard.Extra.Space keyboardModel

                input =
                    { leftPaddleMove = -wasd.y
                    , rightPaddleMove = -arrows.y
                    , pause = space
                    }
            in
                ( { game | input = input, keyboardModel = keyboardModel }, Cmd.none )

        Tick delta ->
            let
                { pause, leftPaddleMove, rightPaddleMove } =
                    game.input

                { state, ball, leftPlayer, rightPlayer, scene } =
                    game

                scoreLeft =
                    if ball.x > 128 then
                        1
                    else
                        0

                scoreRight =
                    if ball.x < 0 then
                        1
                    else
                        0

                pointScored =
                    scoreLeft + scoreRight > 0

                state_ =
                    if pointScored then
                        Pause
                    else if pause then
                        Play
                    else
                        state

                ball_ =
                    if state == Pause then
                        ball
                    else
                        stepBall delta ball leftPlayer rightPlayer

                leftPlayer_ =
                    stepPlyr delta leftPaddleMove scoreLeft leftPlayer

                rightPlayer_ =
                    stepPlyr delta rightPaddleMove scoreRight rightPlayer

                scene_ =
                    scene
                        |> updateSprite ball_
                        |> updateSprite leftPlayer_
                        |> updateSprite rightPlayer_
            in
                { game
                    | state = state_
                    , ball = ball_
                    , leftPlayer = leftPlayer_
                    , rightPlayer = rightPlayer_
                    , scene = scene_
                }
                    ! []


updateSprite : Elmo8.Scene.Sprite a -> Elmo8.Scene.Model -> Elmo8.Scene.Model
updateSprite sprite model =
    Elmo8.Scene.updateSprite model sprite



--  VIEW


view : Game -> Html.Html Msg
view game =
    div []
        [ Elmo8.Scene.render game.display game.assets game.scene
            |> Elmo8.GL.Display.view game.display
            |> Html.map DisplayMsg
        , div
            [ style
                [ ( "color", "black" )
                , ( "text-align", "center" )
                ]
            ]
            [ h1 [] [ text ((toString game.leftPlayer.score) ++ " : " ++ (toString game.rightPlayer.score)) ]
            , text "Use W/S for left player, ↑/↓ for right player & space-bar to serve the ball."
            ]
        ]



--  SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , AnimationFrame.diffs ((\dt -> dt / 6000) >> Tick)
        ]



--  Main
-- main : Config Game -> Program Never (Model Game) Msg


main : Program Never Game Msg
main =
    Html.program
        { init = initialGame
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--  Helper Functions
--  are n and m near each other?
--  specifically are they within c of each other?


near : Int -> Int -> Int -> Bool
near n c m =
    m >= n - c && m <= n + c



--  Within?
--  is the ball within a paddle?


(<?>) : Ball -> Player -> Bool
(<?>) ball player =
    near player.x 2 ball.x
        && near player.y 8 ball.y



-- change the direction of a velocity based on collisions


stepV : Int -> Bool -> Bool -> Int
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v
    else if upperCollision then
        -(abs v)
    else
        v



-- step the position of an object based on its velocity and a timestep


stepObj : Time -> Object a -> Object a
stepObj t ({ x, y, vx, vy } as obj) =
    let
        deltaX =
            round (toFloat vx * t)

        deltaY =
            round (toFloat vy * t)
    in
        { obj
            | x = x + deltaX
            , y = y + deltaY
        }



-- move a ball forward, detecting collisions with either paddle


stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({ x, y, vx, vy } as ball) player1 player2 =
    if not (ball.x |> near 64 64) then
        { ball | x = 64, y = 64 }
    else
        stepObj t
            { ball
                | vx =
                    stepV vx (ball <?> player1) (ball <?> player2)
                , vy =
                    stepV vy (y < 4) (y > 120)
            }



-- step a player forward, making sure it does not fly off the court


stepPlyr : Time -> Int -> Int -> Player -> Player
stepPlyr t paddleMove points player =
    let
        player_ =
            stepObj t { player | vy = paddleMove * 200 }

        y_ =
            clamp 0 120 player_.y

        score_ =
            player.score + points
    in
        { player_ | y = y_, score = score_ }
