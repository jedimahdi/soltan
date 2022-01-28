module View.Card exposing (..)

import Asset exposing (Image)
import Data.Card as Card exposing (Card, Suit(..), Value(..))
import Element exposing (Element, el, height, px, width)
import Element.Events as Events


list : List (Element.Attribute msg) -> { onClick : Maybe (Card -> msg), cards : List Card } -> Element msg
list attrs { onClick, cards } =
    Element.wrappedRow attrs <| List.map (item onClick) cards


image : { src : Image, description : String, onClick : Maybe (Card -> msg), card : Card } -> Element msg
image data =
    case data.onClick of
        Nothing ->
            Asset.img [] { src = data.src, description = data.description }

        Just onClick ->
            Asset.img [ Events.onClick (onClick data.card), Element.pointer ] { src = data.src, description = data.description }


item : Maybe (Card -> msg) -> Card -> Element msg
item onClick card =
    case ( card.suit, card.value ) of
        ( Club, Two ) ->
            image { src = Asset.clubs2, description = "Two of clubs", onClick = onClick, card = card }

        ( Club, Three ) ->
            image { src = Asset.clubs3, description = "Three of clubs", onClick = onClick, card = card }

        ( Club, Four ) ->
            image { src = Asset.clubs4, description = "Four of clubs", onClick = onClick, card = card }

        ( Club, Five ) ->
            image { src = Asset.clubs5, description = "Five of clubs", onClick = onClick, card = card }

        ( Club, Six ) ->
            image { src = Asset.clubs6, description = "Six of clubs", onClick = onClick, card = card }

        ( Club, Seven ) ->
            image { src = Asset.clubs7, description = "Seven of clubs", onClick = onClick, card = card }

        ( Club, Eight ) ->
            image { src = Asset.clubs8, description = "Eight of clubs", onClick = onClick, card = card }

        ( Club, Nine ) ->
            image { src = Asset.clubs9, description = "Nine of clubs", onClick = onClick, card = card }

        ( Club, Ten ) ->
            image { src = Asset.clubs10, description = "Ten of clubs", onClick = onClick, card = card }

        ( Club, Jack ) ->
            image { src = Asset.clubsj, description = "Jack of clubs", onClick = onClick, card = card }

        ( Club, Queen ) ->
            image { src = Asset.clubsq, description = "Queen of clubs", onClick = onClick, card = card }

        ( Club, King ) ->
            image { src = Asset.clubsk, description = "King of clubs", onClick = onClick, card = card }

        ( Club, Ace ) ->
            image { src = Asset.clubsa, description = "Ace of clubs", onClick = onClick, card = card }

        ( Diamond, Two ) ->
            image { src = Asset.diamonds2, description = "Two of diamonds", onClick = onClick, card = card }

        ( Diamond, Three ) ->
            image { src = Asset.diamonds3, description = "Three of diamonds", onClick = onClick, card = card }

        ( Diamond, Four ) ->
            image { src = Asset.diamonds4, description = "Four of diamonds", onClick = onClick, card = card }

        ( Diamond, Five ) ->
            image { src = Asset.diamonds5, description = "Five of diamonds", onClick = onClick, card = card }

        ( Diamond, Six ) ->
            image { src = Asset.diamonds6, description = "Six of diamonds", onClick = onClick, card = card }

        ( Diamond, Seven ) ->
            image { src = Asset.diamonds7, description = "Seven of diamonds", onClick = onClick, card = card }

        ( Diamond, Eight ) ->
            image { src = Asset.diamonds8, description = "Eight of diamonds", onClick = onClick, card = card }

        ( Diamond, Nine ) ->
            image { src = Asset.diamonds9, description = "Nine of diamonds", onClick = onClick, card = card }

        ( Diamond, Ten ) ->
            image { src = Asset.diamonds10, description = "Ten of diamonds", onClick = onClick, card = card }

        ( Diamond, Jack ) ->
            image { src = Asset.diamondsj, description = "Jack of diamonds", onClick = onClick, card = card }

        ( Diamond, Queen ) ->
            image { src = Asset.diamondsq, description = "Queen of diamonds", onClick = onClick, card = card }

        ( Diamond, King ) ->
            image { src = Asset.diamondsk, description = "King of diamonds", onClick = onClick, card = card }

        ( Diamond, Ace ) ->
            image { src = Asset.diamondsa, description = "Ace of diamonds", onClick = onClick, card = card }

        ( Heart, Two ) ->
            image { src = Asset.hearts2, description = "Two of hearts", onClick = onClick, card = card }

        ( Heart, Three ) ->
            image { src = Asset.hearts3, description = "Three of hearts", onClick = onClick, card = card }

        ( Heart, Four ) ->
            image { src = Asset.hearts4, description = "Four of hearts", onClick = onClick, card = card }

        ( Heart, Five ) ->
            image { src = Asset.hearts5, description = "Five of hearts", onClick = onClick, card = card }

        ( Heart, Six ) ->
            image { src = Asset.hearts6, description = "Six of hearts", onClick = onClick, card = card }

        ( Heart, Seven ) ->
            image { src = Asset.hearts7, description = "Seven of hearts", onClick = onClick, card = card }

        ( Heart, Eight ) ->
            image { src = Asset.hearts8, description = "Eight of hearts", onClick = onClick, card = card }

        ( Heart, Nine ) ->
            image { src = Asset.hearts9, description = "Nine of hearts", onClick = onClick, card = card }

        ( Heart, Ten ) ->
            image { src = Asset.hearts10, description = "Ten of hearts", onClick = onClick, card = card }

        ( Heart, Jack ) ->
            image { src = Asset.heartsj, description = "Jack of hearts", onClick = onClick, card = card }

        ( Heart, Queen ) ->
            image { src = Asset.heartsq, description = "Queen of hearts", onClick = onClick, card = card }

        ( Heart, King ) ->
            image { src = Asset.heartsk, description = "King of hearts", onClick = onClick, card = card }

        ( Heart, Ace ) ->
            image { src = Asset.heartsa, description = "Ace of hearts", onClick = onClick, card = card }

        ( Spade, Two ) ->
            image { src = Asset.spades2, description = "Two of spades", onClick = onClick, card = card }

        ( Spade, Three ) ->
            image { src = Asset.spades3, description = "Three of spades", onClick = onClick, card = card }

        ( Spade, Four ) ->
            image { src = Asset.spades4, description = "Four of spades", onClick = onClick, card = card }

        ( Spade, Five ) ->
            image { src = Asset.spades5, description = "Five of spades", onClick = onClick, card = card }

        ( Spade, Six ) ->
            image { src = Asset.spades6, description = "Six of spades", onClick = onClick, card = card }

        ( Spade, Seven ) ->
            image { src = Asset.spades7, description = "Seven of spades", onClick = onClick, card = card }

        ( Spade, Eight ) ->
            image { src = Asset.spades8, description = "Eight of spades", onClick = onClick, card = card }

        ( Spade, Nine ) ->
            image { src = Asset.spades9, description = "Nine of spades", onClick = onClick, card = card }

        ( Spade, Ten ) ->
            image { src = Asset.spades10, description = "Ten of spades", onClick = onClick, card = card }

        ( Spade, Jack ) ->
            image { src = Asset.spadesj, description = "Jack of spades", onClick = onClick, card = card }

        ( Spade, Queen ) ->
            image { src = Asset.spadesq, description = "Queen of spades", onClick = onClick, card = card }

        ( Spade, King ) ->
            image { src = Asset.spadesk, description = "King of spades", onClick = onClick, card = card }

        ( Spade, Ace ) ->
            image { src = Asset.spadesa, description = "Ace of spades", onClick = onClick, card = card }


large : Maybe (Card -> msg) -> Card -> Element msg
large onClick card =
    case ( card.suit, card.value ) of
        ( Club, Two ) ->
            image { src = Asset.largeClubs2, description = "Two of clubs", onClick = onClick, card = card }

        ( Club, Three ) ->
            image { src = Asset.largeClubs3, description = "Three of clubs", onClick = onClick, card = card }

        ( Club, Four ) ->
            image { src = Asset.largeClubs4, description = "Four of clubs", onClick = onClick, card = card }

        ( Club, Five ) ->
            image { src = Asset.largeClubs5, description = "Five of clubs", onClick = onClick, card = card }

        ( Club, Six ) ->
            image { src = Asset.largeClubs6, description = "Six of clubs", onClick = onClick, card = card }

        ( Club, Seven ) ->
            image { src = Asset.largeClubs7, description = "Seven of clubs", onClick = onClick, card = card }

        ( Club, Eight ) ->
            image { src = Asset.largeClubs8, description = "Eight of clubs", onClick = onClick, card = card }

        ( Club, Nine ) ->
            image { src = Asset.largeClubs9, description = "Nine of clubs", onClick = onClick, card = card }

        ( Club, Ten ) ->
            image { src = Asset.largeClubs10, description = "Ten of clubs", onClick = onClick, card = card }

        ( Club, Jack ) ->
            image { src = Asset.largeClubsj, description = "Jack of clubs", onClick = onClick, card = card }

        ( Club, Queen ) ->
            image { src = Asset.largeClubsq, description = "Queen of clubs", onClick = onClick, card = card }

        ( Club, King ) ->
            image { src = Asset.largeClubsk, description = "King of clubs", onClick = onClick, card = card }

        ( Club, Ace ) ->
            image { src = Asset.largeClubsa, description = "Ace of clubs", onClick = onClick, card = card }

        ( Diamond, Two ) ->
            image { src = Asset.largeDiamonds2, description = "Two of diamonds", onClick = onClick, card = card }

        ( Diamond, Three ) ->
            image { src = Asset.largeDiamonds3, description = "Three of diamonds", onClick = onClick, card = card }

        ( Diamond, Four ) ->
            image { src = Asset.largeDiamonds4, description = "Four of diamonds", onClick = onClick, card = card }

        ( Diamond, Five ) ->
            image { src = Asset.largeDiamonds5, description = "Five of diamonds", onClick = onClick, card = card }

        ( Diamond, Six ) ->
            image { src = Asset.largeDiamonds6, description = "Six of diamonds", onClick = onClick, card = card }

        ( Diamond, Seven ) ->
            image { src = Asset.largeDiamonds7, description = "Seven of diamonds", onClick = onClick, card = card }

        ( Diamond, Eight ) ->
            image { src = Asset.largeDiamonds8, description = "Eight of diamonds", onClick = onClick, card = card }

        ( Diamond, Nine ) ->
            image { src = Asset.largeDiamonds9, description = "Nine of diamonds", onClick = onClick, card = card }

        ( Diamond, Ten ) ->
            image { src = Asset.largeDiamonds10, description = "Ten of diamonds", onClick = onClick, card = card }

        ( Diamond, Jack ) ->
            image { src = Asset.largeDiamondsj, description = "Jack of diamonds", onClick = onClick, card = card }

        ( Diamond, Queen ) ->
            image { src = Asset.largeDiamondsq, description = "Queen of diamonds", onClick = onClick, card = card }

        ( Diamond, King ) ->
            image { src = Asset.largeDiamondsk, description = "King of diamonds", onClick = onClick, card = card }

        ( Diamond, Ace ) ->
            image { src = Asset.largeDiamondsa, description = "Ace of diamonds", onClick = onClick, card = card }

        ( Heart, Two ) ->
            image { src = Asset.largeHearts2, description = "Two of hearts", onClick = onClick, card = card }

        ( Heart, Three ) ->
            image { src = Asset.largeHearts3, description = "Three of hearts", onClick = onClick, card = card }

        ( Heart, Four ) ->
            image { src = Asset.largeHearts4, description = "Four of hearts", onClick = onClick, card = card }

        ( Heart, Five ) ->
            image { src = Asset.largeHearts5, description = "Five of hearts", onClick = onClick, card = card }

        ( Heart, Six ) ->
            image { src = Asset.largeHearts6, description = "Six of hearts", onClick = onClick, card = card }

        ( Heart, Seven ) ->
            image { src = Asset.largeHearts7, description = "Seven of hearts", onClick = onClick, card = card }

        ( Heart, Eight ) ->
            image { src = Asset.largeHearts8, description = "Eight of hearts", onClick = onClick, card = card }

        ( Heart, Nine ) ->
            image { src = Asset.largeHearts9, description = "Nine of hearts", onClick = onClick, card = card }

        ( Heart, Ten ) ->
            image { src = Asset.largeHearts10, description = "Ten of hearts", onClick = onClick, card = card }

        ( Heart, Jack ) ->
            image { src = Asset.largeHeartsj, description = "Jack of hearts", onClick = onClick, card = card }

        ( Heart, Queen ) ->
            image { src = Asset.largeHeartsq, description = "Queen of hearts", onClick = onClick, card = card }

        ( Heart, King ) ->
            image { src = Asset.largeHeartsk, description = "King of hearts", onClick = onClick, card = card }

        ( Heart, Ace ) ->
            image { src = Asset.largeHeartsa, description = "Ace of hearts", onClick = onClick, card = card }

        ( Spade, Two ) ->
            image { src = Asset.largeSpades2, description = "Two of spades", onClick = onClick, card = card }

        ( Spade, Three ) ->
            image { src = Asset.largeSpades3, description = "Three of spades", onClick = onClick, card = card }

        ( Spade, Four ) ->
            image { src = Asset.largeSpades4, description = "Four of spades", onClick = onClick, card = card }

        ( Spade, Five ) ->
            image { src = Asset.largeSpades5, description = "Five of spades", onClick = onClick, card = card }

        ( Spade, Six ) ->
            image { src = Asset.largeSpades6, description = "Six of spades", onClick = onClick, card = card }

        ( Spade, Seven ) ->
            image { src = Asset.largeSpades7, description = "Seven of spades", onClick = onClick, card = card }

        ( Spade, Eight ) ->
            image { src = Asset.largeSpades8, description = "Eight of spades", onClick = onClick, card = card }

        ( Spade, Nine ) ->
            image { src = Asset.largeSpades9, description = "Nine of spades", onClick = onClick, card = card }

        ( Spade, Ten ) ->
            image { src = Asset.largeSpades10, description = "Ten of spades", onClick = onClick, card = card }

        ( Spade, Jack ) ->
            image { src = Asset.largeSpadesj, description = "Jack of spades", onClick = onClick, card = card }

        ( Spade, Queen ) ->
            image { src = Asset.largeSpadesq, description = "Queen of spades", onClick = onClick, card = card }

        ( Spade, King ) ->
            image { src = Asset.largeSpadesk, description = "King of spades", onClick = onClick, card = card }

        ( Spade, Ace ) ->
            image { src = Asset.largeSpadesa, description = "Ace of spades", onClick = onClick, card = card }
