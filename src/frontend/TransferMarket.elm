module TransferMarket exposing (view, update)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import RootMsg
import Types exposing (..)
import TransferMarketTypes exposing (Msg, State, Msg(ViewListing, ViewAll, UpdateBidInput, MakeBid, WithdrawBid),
                                     View(ListView, PlayerView))
import PlayerDetailedView
import Styles
import Uitk
import Utils
import ClientServer

view : TeamId -> State -> Html Msg
view ownTeamId state = case state.view of
    PlayerView pvstate ->
        let bidInput = div [] [
                Uitk.actionButton MakeBid "Make bid",
                input [ type_ "number", step "10000", value <| toString pvstate.bidInputValue, onInput <| UpdateBidInput ] [],
                Uitk.actionButton (UpdateBidInput <| toString <| pvstate.bidInputValue + 10000) "+",
                Uitk.actionButton (UpdateBidInput <| toString <| pvstate.bidInputValue - 10000) "-"
            ]
            withdrawButton = div [] [ Uitk.actionButton WithdrawBid "Withdraw bid" ]
        in Uitk.view (Just <| Uitk.backButton ViewAll) "Transfer Listing" [
                PlayerDetailedView.view pvstate.listing.player,
                div [class "half-width"] [
                    div [Styles.defaultMargin] [
                        case pvstate.listing.status of
                        OnSale -> div [] ([
                                text <| "Accepting offers over " ++ Utils.moneyFormat pvstate.listing.minPrice,
                                bidInput
                            ] ++
                            case pvstate.listing.youBid of
                                Nothing -> []
                                Just amount -> [div [] [
                                    text <| "Your current bid is " ++ Utils.moneyFormat amount,
                                    withdrawButton
                                ] ]
                        )
                        YouWon -> div [] [text "You have signed this player."]
                        YouLost -> div [] [text "You were outbid by another team or the player was not interested in playing for your team."]
                        Sold -> div [] [text "This player has been sold."]
                        Unsold -> div [] [text "No winning bid."]
                    ]
                ]
            ]

    ListView ->
        let
            listingToTr : TransferListing -> Html Msg
            listingToTr listing =
                let clickAction = if listing.sellerTeamId == ownTeamId then [] else [onClick <| ViewListing listing]

                in Html.tr clickAction [
                    Html.td [] [text <| listing.player.name],
                    Html.td [] [Uitk.playerPositionBadge listing.player],
                    Html.td [] [text <| Utils.moneyFormat listing.minPrice],
                    Html.td [] [text <| case listing.status of
                        OnSale -> Utils.timeFormatShort listing.deadline
                        YouWon -> "You won!"
                        YouLost -> "You lost"
                        Sold -> "Sold"
                        Unsold -> "Unsold"
                    ],
                    Html.td [] [text <|
                        if listing.sellerTeamId == ownTeamId then
                            "You are seller"
                        else case listing.youBid of
                            Just amount -> Utils.moneyFormat amount
                            Nothing -> ""
                    ],
                    Html.td [] [text <| Types.playerAvgSkill listing.player],
                    Html.td [] [text <| toString <| listing.player.shooting],
                    Html.td [] [text <| toString <| listing.player.passing],
                    Html.td [] [text <| toString <| listing.player.tackling],
                    Html.td [] [text <| toString <| listing.player.handling],
                    Html.td [] [text <| toString <| listing.player.speed]
                ]
        in
            Uitk.view Nothing "Transfer Market" [
                Html.table [class "transfer-listings"] (
                    (Html.tr [] [
                        Html.th [] [text "Name"]
                      , Html.th [] [text "Pos"]
                      , Html.th [] [text "Min Price"]
                      , Html.th [] [text "Deadline"]
                      , Html.th [] [text "Your bid"]
                      , Html.th [] [text "Avg."]
                      , Html.th [] [text "Sh"]
                      , Html.th [] [text "Pa"]
                      , Html.th [] [text "Ta"]
                      , Html.th [] [text "Ha"]
                      , Html.th [] [text "Sp"]
                    ]) :: 
                    (List.map listingToTr state.listings)
                )
            ]

makeBid : State -> Int -> TransferListing -> State
makeBid state amount listing =
    let updatedListing = { listing | youBid = Just amount }
        updatedListings = List.map (\l -> if l.id == listing.id then updatedListing else l) state.listings
    in { state | listings = updatedListings,
                 view = PlayerView { listing = updatedListing, bidInputValue = amount } }

withdrawBid : State -> TransferListing -> State
withdrawBid state listing =
    let updatedListing = { listing | youBid = Nothing }
        updatedListings = List.map (\l -> if l.id == listing.id then updatedListing else l) state.listings
    in { state | listings = updatedListings,
                 view = PlayerView { listing = updatedListing, bidInputValue = listing.minPrice } }

update : Msg -> State -> (State, Cmd RootMsg.Msg)
update msg state = case msg of
    WithdrawBid ->
        case state.view of
            PlayerView pvstate -> (withdrawBid state pvstate.listing,
                                   ClientServer.makeTransferBid pvstate.listing.id Nothing)
            _ -> (state, Cmd.none)
    MakeBid ->
        case state.view of
            PlayerView pvstate -> (makeBid state pvstate.bidInputValue pvstate.listing,
                                   ClientServer.makeTransferBid pvstate.listing.id (Just pvstate.bidInputValue))
            _ -> (state, Cmd.none)
    UpdateBidInput amount ->
        case state.view of
            PlayerView pvstate ->
                case String.toInt amount of
                    Ok amount -> ({ state | view = PlayerView { pvstate | bidInputValue = Basics.max pvstate.listing.minPrice amount } }, Cmd.none)
                    Err result -> ({ state | view = PlayerView pvstate }, Cmd.none)
            _ -> (state, Cmd.none)
        {-
        Ok amount -> (makeBid state amount pvstate, Cmd.none)
        Err result -> ({ state | view = PlayerView pvstate }, Cmd.none)
        -}
    ViewAll -> ({ state | view = ListView }, Cmd.none)
    ViewListing listing -> ({ state | view = PlayerView {listing=listing, bidInputValue=listing.minPrice}}, Cmd.none)
