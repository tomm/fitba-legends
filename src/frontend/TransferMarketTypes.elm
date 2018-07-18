module TransferMarketTypes exposing (..)
import Http

import Types

type alias PlayerViewState = { listing: Types.TransferListing, bidInputValue: Int }
type View = ListView | PlayerView PlayerViewState
type alias State = { view: View, listings: List Types.TransferListing }

type Msg = ViewListing Types.TransferListing | ViewAll | UpdateBidInput String | MakeBid | WithdrawBid
